{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GrandPA.Enttec (Widget, withWidget, sendDMX) where

import Control.Exception (bracket)
import Control.Monad (filterM, void)
import Data.Maybe (listToMaybe)
import Data.Serialize (Serialize(..), encode)
import Data.Serialize.Get (getWord8, getWord16le, getByteString)
import Data.Serialize.Put (putWord8, putWord16le, putByteString)
import Data.Word (Word8)
import Foreign.C.String (castCUCharToChar)
import Foreign.C.Types (CInt(..), CUInt, CUChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray0, peekArray0)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (Storable(..))

import qualified Data.ByteString as BS

#include <ftdi.h>

#c
typedef struct ftdi_context ftdi_context_t;
typedef struct ftdi_device_list ftdi_device_list_t;
typedef struct libusb_device libusb_device_t;
#endc

data FtdiContext = FtdiContext Int
                   deriving Show

type UsbDevice = Ptr ()

instance Storable FtdiContext where
    sizeOf _ = {#sizeof ftdi_context_t#}
    alignment = sizeOf
    poke _ = fail "No support for poking FTDI contexts"
    peek p = do
        rbr <- {#get ftdi_context_t->readbuffer_remaining#} p
        return . FtdiContext $ fromIntegral rbr

instance Storable [UsbDevice] where
    sizeOf _ = {#sizeof ftdi_device_list_t#}
    alignment = sizeOf
    poke _ = fail "No support for poking USB device lists"
    peek p = do
        nextPtr <- {#get ftdi_device_list_t->next#} p
        devPtr <- {#get ftdi_device_list_t->dev#} p
        if nextPtr == nullPtr then return [devPtr] else do
            next <- peek $ castPtr nextPtr
            return $ (devPtr : next)

foreign import ccall unsafe "ftdi_new" ftdiNew
    :: IO (Ptr FtdiContext)

foreign import ccall unsafe "ftdi_free" ftdiFree
    :: Ptr FtdiContext -> IO ()

foreign import ccall unsafe "ftdi_usb_find_all" ftdiFindAll
    :: Ptr FtdiContext -> Ptr [UsbDevice] -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "ftdi_usb_get_strings" ftdiGetStrings
    :: Ptr FtdiContext -> UsbDevice
    -> Ptr CUChar -> CInt
    -> Ptr CUChar -> CInt
    -> Ptr CUChar -> CInt
    -> IO CInt

foreign import ccall unsafe "ftdi_usb_open_dev" ftdiOpen
    :: Ptr FtdiContext -> UsbDevice -> IO CInt

foreign import ccall unsafe "ftdi_usb_close" ftdiClose
    :: Ptr FtdiContext -> IO CInt

foreign import ccall unsafe "ftdi_write_data" ftdiWrite
    :: Ptr FtdiContext -> Ptr CUChar -> CInt -> IO CInt

newtype Widget = Widget (Ptr FtdiContext)

data WidgetDataType = WidgetReprogramFirmware
                    | WidgetFlashPage
                    | WidgetGetParameters
                    | WidgetSetParameters
                    | WidgetReceivedDMX
                    | WidgetSendDMX
                    | WidgetSendRDM
                    | WidgetReceiveDMXOnChange
                    | WidgetChangeOfState
                    | WidgetGetSerial
                    | WidgetSendRDMDiscovery
                    | WidgetUnknown
                    deriving Show

instance Serialize WidgetDataType where
    get = let l 2  = WidgetFlashPage
              l 3  = WidgetGetParameters
              l 5  = WidgetReceivedDMX
              l 9  = WidgetChangeOfState
              l 10 = WidgetGetSerial
              l _  = WidgetUnknown
              in fmap l getWord8

    put WidgetReprogramFirmware  = putWord8 1
    put WidgetFlashPage          = putWord8 2
    put WidgetGetParameters      = putWord8 3
    put WidgetSetParameters      = putWord8 4
    put WidgetSendDMX            = putWord8 6
    put WidgetSendRDM            = putWord8 7
    put WidgetReceiveDMXOnChange = putWord8 8
    put WidgetGetSerial          = putWord8 10
    put WidgetSendRDMDiscovery   = putWord8 11
    put _                        = fail "Invalid widget label"

data WidgetData = WidgetData WidgetDataType BS.ByteString
                  deriving Show

instance Serialize WidgetData where
    get = do
        0x7e <- getWord8
        dataType <- get
        dataLen  <- getWord16le
        content <- getByteString $ fromIntegral dataLen
        0xe7 <- getWord8
        return $ WidgetData dataType content

    put (WidgetData dataType content) = do
        putWord8 0x7e
        put dataType
        putWord16le . fromIntegral $ BS.length content
        putByteString content
        putWord8 0xe7

getDeviceSerial :: Ptr FtdiContext -> UsbDevice -> IO String
getDeviceSerial ctx ud = allocaArray0 200 $ \bufSerial -> do
    void $ ftdiGetStrings ctx ud nullPtr 0 nullPtr 0 bufSerial 200
    fmap (map castCUCharToChar) $ peekArray0 0 bufSerial

listFtdiDevices :: Ptr FtdiContext -> IO [UsbDevice]
listFtdiDevices ctx = alloca $ \devlist -> do
    void $ ftdiFindAll ctx devlist 0x0403 0x6001
    peek devlist

findEnttecDevices :: Ptr FtdiContext -> [UsbDevice] -> IO [UsbDevice]
findEnttecDevices ctx =
    filterM (fmap matchSerial . getDeviceSerial ctx)
  where
    -- XXX: Better way to match an Enttec device?
    matchSerial :: String -> Bool
    matchSerial ('A':'7':'0':'0':_) = True
    matchSerial ('E':'N':_)         = True
    matchSerial _                   = False

findFirstDevice :: Ptr FtdiContext -> IO (Maybe UsbDevice)
findFirstDevice ctx = do
    allDevs <- listFtdiDevices ctx
    fmap listToMaybe $ findEnttecDevices ctx allDevs

openWidget :: IO Widget
openWidget = do
    ctx <- ftdiNew
    maybeDev <- findFirstDevice ctx
    case maybeDev of
        Just dev -> do
            result <- ftdiOpen ctx dev
            case result of
                 0 -> return $ Widget ctx
                 _ -> fail "Unable to open DMX widget!"
        Nothing -> fail "Unable to find Enttec device!"

closeWidget :: Widget -> IO ()
closeWidget (Widget ctx) = do
    void $ ftdiClose ctx
    void $ ftdiFree ctx

sendWidget :: Widget -> WidgetData -> IO Bool
sendWidget (Widget ctx) wd =
    BS.useAsCStringLen (encode wd) $ \(content, len) -> do
        size <- ftdiWrite ctx (castPtr content) (fromIntegral len)
        return $ size > 0 && size == fromIntegral len

withWidget :: (Widget -> IO a) -> IO a
withWidget = bracket openWidget closeWidget

sendDMX :: Widget -> Word8 -> [Word8] -> IO ()
sendDMX widget startOffset dmxData =
    void $ sendWidget widget $ WidgetData WidgetSendDMX $
        BS.cons startOffset $ BS.pack dmxData
