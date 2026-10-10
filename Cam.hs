{-# LANGUAGE LambdaCase #-}
import OpenCV as CV
import OpenCV.Unsafe as CV
import OpenCV.VideoIO.Types as CV
import Control.Monad (unless)
import System.Environment

main :: IO ()
main = do
    cap <- createCaptureArg
    w <- CV.videoCaptureGetI cap VideoCapPropFrameWidth
    h <- CV.videoCaptureGetI cap VideoCapPropFrameHeight
    CV.withWindow "video" $ loop cap
  where
    loop cap window = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          CV.imshow window (CV.unsafeCoerceMat img)
          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap window

-- | Use first argument to the program to create capture device
-- it should be usefull for creating demos. Numbers are for cameras and
-- files for videos
createCaptureArg :: IO CV.VideoCapture
createCaptureArg =
   getArgs >>= \case
      []      -> createCapture "0"
      (inp:_) -> createCapture inp
    >>= maybe (error "could not open capture input ") return

createCapture :: String  -> IO (Maybe CV.VideoCapture)
createCapture input = do
    cap <- CV.newVideoCapture
    let openSource src = CV.exceptErrorIO $ CV.videoCaptureOpen cap src
    if input `elem` ["0","1","2","3","4","5","6","7","8","9"]
      then openSource $ CV.VideoDeviceSource (read input) Nothing
      else openSource $ CV.VideoFileSource input Nothing
    isOpened <- CV.videoCaptureIsOpened cap
    if isOpened
      then return $ Just cap
      else return Nothing
