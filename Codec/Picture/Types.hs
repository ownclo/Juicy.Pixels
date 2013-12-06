{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
-- | Module providing the basic types for image manipulation in the library.
-- Defining the types used to store all those _Juicy Pixels_
module Codec.Picture.Types( -- * Types
                            -- ** Image types
                            Image( .. )
                          , MutableImage( .. )
                          , DynamicImage( .. )

                            -- ** Image functions
                          , freezeImage
                          , unsafeFreezeImage

                            -- ** Pixel types
                          , Pixel8
                          , PixelRGB8( .. )
                          , PixelYCbCr8( .. )

                          -- * Type classes
                          , ColorConvertible( .. )
                          , Pixel(..)
                          , ColorSpaceConvertible( .. )

                            -- * Helper functions
                          , pixelMap
                          , pixelFold
                          , dynamicMap
                          , dynamicPixelMap
                          , withImage
                          , generateImage
                          , generateFoldImage

                            -- * Color plane extraction
                          , ColorPlane ( )

                          , PlaneRed( .. )
                          , PlaneGreen( .. )
                          , PlaneBlue( .. )
                          , PlaneLuma( .. )
                          , PlaneCr( .. )
                          , PlaneCb( .. )

                          , extractComponent
                          , unsafeExtractComponent
                          ) where

import Control.Monad( forM_, foldM, liftM, ap )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST( runST )
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Foreign.Storable ( Storable )
import Data.Bits( unsafeShiftL, unsafeShiftR )
import Data.Word( Word8 )
import Data.List( foldl' )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

-- | Image or pixel buffer, the coordinates are assumed to start
-- from the upper-left corner of the image, with the horizontal
-- position first, then the vertical one.
data Image a = Image
    { -- | Width of the image in pixels
      imageWidth  :: {-# UNPACK #-} !Int
      -- | Height of the image in pixels.
    , imageHeight :: {-# UNPACK #-} !Int

      -- | The real image, to extract pixels at some position
      -- you should use the helpers functions.
    , imageData   :: V.Vector (PixelBaseComponent a)
    }

{-# INLINE (!!!) #-}
(!!!) :: (Storable e) => V.Vector e -> Int -> e
(!!!) = V.unsafeIndex

-- | Class used to describle plane present in the pixel
-- type. If a pixel has a plane description associated,
-- you can use the plane name to extract planes independently.
class ColorPlane pixel planeToken where
    -- | Retrieve the index of the component in the
    -- given pixel type.
    toComponentIndex :: pixel -> planeToken -> Int

-- | Define the plane for the red color component
data PlaneRed = PlaneRed

-- | Define the plane for the green color component
data PlaneGreen = PlaneGreen

-- | Define the plane for the blue color component
data PlaneBlue = PlaneBlue

-- | Define the plane for the luma component
data PlaneLuma = PlaneLuma

-- | Define the plane for the Cr component
data PlaneCr = PlaneCr

-- | Define the plane for the Cb component
data PlaneCb = PlaneCb

-- | Extract a color plane from an image given a present plane in the image
-- examples :
--
-- @
--  extractRedPlane :: Image PixelRGB8-> Image Pixel8
--  extractRedPlane = extractComponent PlaneRed
-- @
--
extractComponent :: forall px plane. ( Pixel px
                                     , Pixel (PixelBaseComponent px)
                                     , PixelBaseComponent (PixelBaseComponent px)
                                                    ~ PixelBaseComponent px
                                     , ColorPlane px plane )
                 => plane -> Image px -> Image (PixelBaseComponent px)
extractComponent plane = unsafeExtractComponent idx
    where idx = toComponentIndex (undefined :: px) plane

-- | Extract an image plane of an image, returning an image which
-- can be represented by a gray scale image.
-- If you ask a component out of bound, the `error` function will
-- be called
unsafeExtractComponent :: forall a
                        . ( Pixel a
                          , Pixel (PixelBaseComponent a)
                          , PixelBaseComponent (PixelBaseComponent a)
                                              ~ PixelBaseComponent a)
                       => Int     -- ^ The component index, beginning at 0 ending at (componentCount - 1)
                       -> Image a -- ^ Source image
                       -> Image (PixelBaseComponent a)
unsafeExtractComponent comp img@(Image { imageWidth = w, imageHeight = h })
  | comp >= padd = error $ "extractComponent : invalid component index ("
                         ++ show comp ++ ", max:" ++ show padd ++ ")"
  | otherwise = Image { imageWidth = w, imageHeight = h, imageData = plane }
      where plane = stride img 1 padd comp
            padd = componentCount (undefined :: a)

stride :: (Storable (PixelBaseComponent a))
       => Image a -> Int -> Int -> Int -> V.Vector (PixelBaseComponent a)
stride Image { imageWidth = w, imageHeight = h, imageData = array }
        run padd firstComponent = runST $ do
    let cell_count = w * h * run
    outArray <- M.new cell_count

    let strideWrite write_idx _ | write_idx == cell_count = return ()
        strideWrite write_idx read_idx = do
            forM_ [0 .. run - 1] $ \i ->
                (outArray .<-. (write_idx + i)) $ array !!! (read_idx + i)
            strideWrite (write_idx + run) (read_idx + padd)

    strideWrite 0 firstComponent
    V.unsafeFreeze outArray

instance NFData (Image a) where
    rnf (Image width height dat) = width  `seq`
                                   height `seq`
                                   dat    `seq`
                                   ()

-- | Image or pixel buffer, the coordinates are assumed to start
-- from the upper-left corner of the image, with the horizontal
-- position first, then the vertical one. The image can be transformed in place.
data MutableImage s a = MutableImage
    { -- | Width of the image in pixels
      mutableImageWidth  :: {-# UNPACK #-} !Int

      -- | Height of the image in pixels.
    , mutableImageHeight :: {-# UNPACK #-} !Int

      -- | The real image, to extract pixels at some position
      -- you should use the helpers functions.
    , mutableImageData   :: M.STVector s (PixelBaseComponent a)
    }

-- | `O(n)` Yield an immutable copy of an image by making a copy of it
freezeImage :: (Storable (PixelBaseComponent a), PrimMonad m)
            => MutableImage (PrimState m) a -> m (Image a)
freezeImage (MutableImage w h d) = Image w h `liftM` V.freeze d

-- | `O(1)` Unsafe convert a mutable image to an immutable one without copying.
-- The mutable image may not be used after this operation.
unsafeFreezeImage ::  (Storable (PixelBaseComponent a), PrimMonad m)
                  => MutableImage (PrimState m) a -> m (Image a)
unsafeFreezeImage (MutableImage w h d) = Image w h `liftM` V.unsafeFreeze d

instance NFData (MutableImage s a) where
    rnf (MutableImage width height dat) = width  `seq`
                                          height `seq`
                                          dat    `seq`
                                          ()

-- | Type allowing the loading of an image with different pixel
-- structures
data DynamicImage =
       -- | An image in true color.
       ImageRGB8  (Image PixelRGB8)
       -- | An image in the colorspace used by Jpeg images.
     | ImageYCbCr8 (Image PixelYCbCr8)

-- | Helper function to help extract information from dynamic
-- image. To get the width of an dynamic image, you can use
-- the following snippet :
--
-- > dynWidth :: DynamicImage -> Int
-- > dynWidth img = dynamicMap imageWidth img
--
dynamicMap :: (forall pixel . (Pixel pixel) => Image pixel -> a)
           -> DynamicImage -> a
dynamicMap f (ImageRGB8  i) = f i
dynamicMap f (ImageYCbCr8 i) = f i

-- | Equivalent of the `pixelMap` function for the dynamic images.
-- You can perform pixel colorspace independant operations with this
-- function.
--
-- For instance, if you wan't to extract a square crop of any image,
-- without caring about colorspace, you can use the following snippet.
--
-- > dynSquare :: DynamicImage -> DynamicImage
-- > dynSquare = dynMap squareImage
-- >
-- > squareImage :: Pixel a => Image a -> Image a
-- > squareImage img = generateImage (\x y -> pixelAt img x y) edge edge
-- >    where edge = min (imageWidth img) (imageHeight img)
--
dynamicPixelMap :: (forall pixel . (Pixel pixel) => Image pixel -> Image pixel)
                -> DynamicImage -> DynamicImage
dynamicPixelMap f = aux
  where
    aux (ImageRGB8  i) = ImageRGB8 (f i)
    aux (ImageYCbCr8 i) = ImageYCbCr8 (f i)

instance NFData DynamicImage where
    rnf (ImageRGB8 img)   = rnf img
    rnf (ImageYCbCr8 img) = rnf img

-- | Simple alias for greyscale value in 8 bits.
type Pixel8 = Word8

-- | Pixel type storing classic pixel on 8 bits
-- Value are stored in the following order :
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGB8 = PixelRGB8 {-# UNPACK #-} !Pixel8 -- Red
                           {-# UNPACK #-} !Pixel8 -- Green
                           {-# UNPACK #-} !Pixel8 -- Blue
               deriving (Eq, Show)

-- | Pixel storing data in the YCbCr colorspace,
-- value are stored in the following order :
--
--  * Y (luminance)
--
--  * Cr
--
--  * Cb
--
data PixelYCbCr8 = PixelYCbCr8 {-# UNPACK #-} !Pixel8 -- Y luminance
                               {-# UNPACK #-} !Pixel8 -- Cr red difference
                               {-# UNPACK #-} !Pixel8 -- Cb blue difference
                 deriving (Eq, Show)


-- | Definition of pixels used in images. Each pixel has a color space, and a representative
-- component (Word8 or Float).
class ( Storable (PixelBaseComponent a)
      , Num (PixelBaseComponent a), Eq a ) => Pixel a where
    -- | Type of the pixel component, "classical" images
    -- would have Word8 type as their PixelBaseComponent,
    -- HDR image would have Float for instance
    type PixelBaseComponent a :: *

    -- | Call the function for every component of the pixels.
    -- For example for RGB pixels mixWith is declared like this :
    --
    -- > mixWith f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
    -- >    PixelRGB8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)
    --
    mixWith :: (Int -> PixelBaseComponent a -> PixelBaseComponent a -> PixelBaseComponent a)
            -> a -> a -> a

    -- | Return the number of component of the pixel
    componentCount :: a -> Int

    -- | Apply a function to all color component of a pixel.
    colorMap :: (PixelBaseComponent a -> PixelBaseComponent a) -> a -> a

    -- | Calculate the index for the begining of the pixel
    pixelBaseIndex :: Image a -> Int -> Int -> Int
    pixelBaseIndex (Image { imageWidth = w }) x y =
            (x + y * w) * componentCount (undefined :: a)

    -- | Calculate theindex for the begining of the pixel at position x y
    mutablePixelBaseIndex :: MutableImage s a -> Int -> Int -> Int
    mutablePixelBaseIndex (MutableImage { mutableImageWidth = w }) x y =
            (x + y * w) * componentCount (undefined :: a)

    -- | Extract a pixel at a given position, (x, y), the origin
    -- is assumed to be at the corner top left, positive y to the
    -- bottom of the image
    pixelAt :: Image a -> Int -> Int -> a

    -- | Same as pixelAt but for mutable images.
    readPixel :: PrimMonad m => MutableImage (PrimState m) a -> Int -> Int -> m a

    -- | Write a pixel in a mutable image at position x y
    writePixel :: PrimMonad m => MutableImage (PrimState m) a -> Int -> Int -> a -> m ()

    -- | Unsafe version of pixelAt, read a pixel at the given
    -- index without bound checking (if possible).
    -- The index is expressed in number (PixelBaseComponent a)
    unsafePixelAt :: V.Vector (PixelBaseComponent a) -> Int -> a

    -- | Unsafe version of readPixel,  read a pixel at the given
    -- position without bound checking (if possible). The index
    -- is expressed in number (PixelBaseComponent a)
    unsafeReadPixel :: PrimMonad m => M.STVector (PrimState m) (PixelBaseComponent a) -> Int -> m a

    -- | Unsafe version of writePixel, write a pixel at the
    -- given position without bound checking. This can be _really_ unsafe.
    -- The index is expressed in number (PixelBaseComponent a)
    unsafeWritePixel :: PrimMonad m => M.STVector (PrimState m) (PixelBaseComponent a) -> Int -> a -> m ()


-- | Implement upcasting for pixel types
-- Minimal declaration declaration `promotePixel`
-- It is strongly recommanded to overload promoteImage to keep
-- performance acceptable
class (Pixel a, Pixel b) => ColorConvertible a b where
    -- | Convert a pixel type to another pixel type. This
    -- operation should never loss any data.
    promotePixel :: a -> b

    -- | Change the underlying pixel type of an image by performing a full copy
    -- of it.
    promoteImage :: Image a -> Image b
    promoteImage = pixelMap promotePixel

-- | This class abstract colorspace conversion. This
-- conversion can be lossy, which ColorConvertible cannot
class (Pixel a, Pixel b) => ColorSpaceConvertible a b where
    -- | Pass a pixel from a colorspace (say RGB) to the second one
    -- (say YCbCr)
    convertPixel :: a -> b

    -- | Helper function to convert a whole image by taking a
    -- copy it.
    convertImage :: Image a -> Image b
    convertImage = pixelMap convertPixel

-- | Create an image given a function to generate pixels.
-- The function will receive value from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinate 0,0 is the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- for example, to create a small gradient image :
--
-- > imageCreator :: String -> Image PixelRGB8
-- > imageCreator path = writePng path $ generateImage pixelRenderer 250 300
-- >    where pixelRenderer x y = PixelRGB8 x y 128
--
generateImage :: forall a. (Pixel a)
              => (Int -> Int -> a)  -- ^ Generating function, with `x` and `y` params.
              -> Int        -- ^ Width in pixels
              -> Int        -- ^ Height in pixels
              -> Image a
generateImage f w h = Image { imageWidth = w, imageHeight = h, imageData = generated }
  where compCount = componentCount (undefined :: a)
        generated = runST $ do
            arr <- M.new (w * h * compCount)
            let lineGenerator _ y | y >= h = return ()
                lineGenerator lineIdx y = column lineIdx 0
                  where column idx x | x >= w = lineGenerator idx $ y + 1
                        column idx x = do
                            unsafeWritePixel arr idx $ f x y
                            column (idx + compCount) $ x + 1

            lineGenerator 0 0
            V.unsafeFreeze arr

-- | Create an image using a monadic initializer function.
-- The function will receive value from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinate 0,0 is the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- The function is called for each pixel in the line from left to right (0 to width - 1)
-- and for each line (0 to height - 1).
withImage :: forall m pixel. (Pixel pixel, PrimMonad m)
          => Int                     -- ^ Image width
          -> Int                     -- ^ Image height
          -> (Int -> Int -> m pixel) -- ^ Generating functions
          -> m (Image pixel)
withImage width height pixelGenerator = do
  let pixelComponentCount = componentCount (undefined :: pixel)
  arr <- M.new (width * height * pixelComponentCount)
  let mutImage = MutableImage
        { mutableImageWidth = width
        , mutableImageHeight = height
        , mutableImageData = arr
        }

  let pixelPositions = [(x, y) | y <- [0 .. height-1], x <- [0..width-1]]
  sequence_ [pixelGenerator x y >>= unsafeWritePixel arr idx
                        | ((x,y), idx) <- zip pixelPositions [0, pixelComponentCount ..]]
  unsafeFreezeImage mutImage

-- | Create an image given a function to generate pixels.
-- The function will receive value from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinate 0,0 is the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- the acc parameter is a user defined one.
--
-- The function is called for each pixel in the line from left to right (0 to width - 1)
-- and for each line (0 to height - 1).
generateFoldImage :: forall a acc. (Pixel a)
                  => (acc -> Int -> Int -> (acc, a)) -- ^ Function taking the state, x and y
                  -> acc        -- ^ Initial state
                  -> Int        -- ^ Width in pixels
                  -> Int        -- ^ Height in pixels
                  -> (acc, Image a)
generateFoldImage f intialAcc w h =
 (finalState, Image { imageWidth = w, imageHeight = h, imageData = generated })
  where compCount = componentCount (undefined :: a)
        (finalState, generated) = runST $ do
            arr <- M.new (w * h * compCount)
            let mutImage = MutableImage {
                                mutableImageWidth = w,
                                mutableImageHeight = h,
                                mutableImageData = arr }
            foldResult <- foldM (\acc (x,y) -> do
                    let (acc', px) = f acc x y
                    writePixel mutImage x y px
                    return acc') intialAcc [(x,y) | y <- [0 .. h-1], x <- [0 .. w-1]]

            frozen <- V.unsafeFreeze arr
            return (foldResult, frozen)

-- | Fold over the pixel of an image with a raster scan order :
-- from top to bottom, left to right
{-# INLINE pixelFold #-}
pixelFold :: (Pixel pixel)
          => (acc -> Int -> Int -> pixel -> acc) -> acc -> Image pixel -> acc
pixelFold f initialAccumulator img@(Image { imageWidth = w, imageHeight = h }) =
  lineFold
    where pixelFolder y acc x = f acc x y $ pixelAt img x y
          columnFold lineAcc y = foldl' (pixelFolder y) lineAcc [0 .. w - 1]
          lineFold = foldl' columnFold initialAccumulator [0 .. h - 1]

-- | `map` equivalent for an image, working at the pixel level.
-- Little example : a brightness function for an rgb image
--
-- > brightnessRGB8 :: Int -> Image PixelRGB8 -> Image PixelRGB8
-- > brightnessRGB8 add = pixelMap brightFunction
-- >      where up v = fromIntegral (fromIntegral v + add)
-- >            brightFunction (PixelRGB8 r g b) =
-- >                    PixelRGB8 (up r) (up g) (up b)
--
pixelMap :: forall a b. (Pixel a, Pixel b)
         => (a -> b) -> Image a -> Image b
{-# RULES "pixelMap fusion" forall g f. pixelMap g . pixelMap f = pixelMap (g . f) #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelYCbCr8 -> PixelRGB8) -> Image PixelYCbCr8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGB8 -> PixelYCbCr8) -> Image PixelRGB8 -> Image PixelYCbCr8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGB8 -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMap :: (Pixel8 -> PixelRGB8) -> Image Pixel8 -> Image PixelRGB8 #-}
pixelMap f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  Image w h pixels
    where sourceComponentCount = componentCount (undefined :: a)
          destComponentCount = componentCount (undefined :: b)

          pixels = runST $ do
            newArr <- M.new (w * h * destComponentCount)
            let lineMapper _ _ y | y >= h = return ()
                lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
                  where colMapper readIdx writeIdx x
                            | x >= w = lineMapper readIdx writeIdx $ y + 1
                            | otherwise = do
                                unsafeWritePixel newArr writeIdx . f $ unsafePixelAt vec readIdx
                                colMapper (readIdx + sourceComponentCount)
                                          (writeIdx + destComponentCount)
                                          (x + 1)
            lineMapper 0 0 0

            -- unsafeFreeze avoids making a second copy and it will be
            -- safe because newArray can't be referenced as a mutable array
            -- outside of this where block
            V.unsafeFreeze newArr

-- | Helper class to help extract a luma plane out
-- of an image or a pixel
class (Pixel a, Pixel (PixelBaseComponent a)) => LumaPlaneExtractable a where
    -- | Compute the luminance part of a pixel
    computeLuma      :: a -> PixelBaseComponent a

    -- | Extract a luma plane out of an image. This
    -- method is in the typeclass to help performant
    -- implementation.
    --
    -- > jpegToGrayScale :: FilePath -> FilePath -> IO ()
    -- > jpegToGrayScale source dest
    extractLumaPlane :: Image a -> Image (PixelBaseComponent a)
    extractLumaPlane = pixelMap computeLuma

instance LumaPlaneExtractable Pixel8 where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

instance LumaPlaneExtractable PixelYCbCr8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelYCbCr8 y _ _) = y
    extractLumaPlane = extractComponent PlaneLuma

-- | Free promotion for identic pixel types
instance (Pixel a) => ColorConvertible a a where
    {-# INLINE promotePixel #-}
    promotePixel = id

    {-# INLINE promoteImage #-}
    promoteImage = id

{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a) => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.read -- unsafeRead

{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a) => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.)  = M.write -- unsafeWrite

--------------------------------------------------
----            Pixel8 instances
--------------------------------------------------
instance Pixel Pixel8 where
    type PixelBaseComponent Pixel8 = Word8

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    componentCount _ = 1
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    unsafePixelAt = V.unsafeIndex
    unsafeReadPixel = M.unsafeRead
    unsafeWritePixel = M.unsafeWrite

instance ColorConvertible Pixel8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGB8 c c c

--------------------------------------------------
----            PixelRGB8 instances
--------------------------------------------------
instance Pixel PixelRGB8 where
    type PixelBaseComponent PixelRGB8 = Word8

    {-# INLINE mixWith #-}
    mixWith f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
        PixelRGB8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGB8 r g b) = PixelRGB8 (f r) (f g) (f b)

    componentCount _ = 3

    pixelAt image@(Image { imageData = arr }) x y = PixelRGB8 (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
                                                              (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        return $ PixelRGB8 rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGB8 rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv

    unsafePixelAt v idx =
        PixelRGB8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    unsafeReadPixel vec idx =
        PixelRGB8 `liftM` M.unsafeRead vec idx
                  `ap` M.unsafeRead vec (idx + 1)
                  `ap` M.unsafeRead vec (idx + 2)
    unsafeWritePixel v idx (PixelRGB8 r g b) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b

instance ColorPlane PixelRGB8 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGB8 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGB8 PlaneBlue where
    toComponentIndex _ _ = 2

instance LumaPlaneExtractable PixelRGB8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGB8 r g b) = floor $ 0.3 * toRational r +
                                            0.59 * toRational g +
                                            0.11 * toRational b

--------------------------------------------------
----            PixelYCbCr8 instances
--------------------------------------------------
instance Pixel PixelYCbCr8 where
    type PixelBaseComponent PixelYCbCr8 = Word8

    {-# INLINE mixWith #-}
    mixWith f (PixelYCbCr8 ya cba cra) (PixelYCbCr8 yb cbb crb) =
        PixelYCbCr8 (f 0 ya yb) (f 1 cba cbb) (f 2 cra crb)

    {-# INLINE colorMap #-}
    colorMap f (PixelYCbCr8 y cb cr) = PixelYCbCr8 (f y) (f cb) (f cr)
    componentCount _ = 3
    pixelAt image@(Image { imageData = arr }) x y = PixelYCbCr8 (arr ! (baseIdx + 0))
                                                                (arr ! (baseIdx + 1))
                                                                (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr .!!!. baseIdx
        cbv <- arr .!!!. (baseIdx + 1)
        crv <- arr .!!!. (baseIdx + 2)
        return $ PixelYCbCr8 yv cbv crv
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYCbCr8 yv cbv crv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) yv
        (arr .<-. (baseIdx + 1)) cbv
        (arr .<-. (baseIdx + 2)) crv

    unsafePixelAt v idx =
        PixelYCbCr8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    unsafeReadPixel vec idx =
        PixelYCbCr8 `liftM` M.unsafeRead vec idx
                    `ap` M.unsafeRead vec (idx + 1)
                    `ap` M.unsafeRead vec (idx + 2)
    unsafeWritePixel v idx (PixelYCbCr8 y cb cr) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) cb
                              >> M.unsafeWrite v (idx + 2) cr

instance (Pixel a) => ColorSpaceConvertible a a where
    convertPixel = id
    convertImage = id

scaleBits, oneHalf :: Int
scaleBits = 16
oneHalf = 1 `unsafeShiftL` (scaleBits - 1)

fix :: Float -> Int
fix x = floor $ x * fromIntegral ((1 :: Int) `unsafeShiftL` scaleBits) + 0.5


rYTab, gYTab, bYTab, rCbTab, gCbTab, bCbTab, gCrTab, bCrTab :: V.Vector Int
rYTab = V.fromListN 256 [fix 0.29900 * i | i <- [0..255] ]
gYTab = V.fromListN 256 [fix 0.58700 * i | i <- [0..255] ]
bYTab = V.fromListN 256 [fix 0.11400 * i + oneHalf | i <- [0..255] ]
rCbTab = V.fromListN 256 [(- fix 0.16874) * i | i <- [0..255] ]
gCbTab = V.fromListN 256 [(- fix 0.33126) * i | i <- [0..255] ]
bCbTab = V.fromListN 256 [fix 0.5 * i + (128 `unsafeShiftL` scaleBits) + oneHalf - 1| i <- [0..255] ]
gCrTab = V.fromListN 256 [(- fix 0.41869) * i | i <- [0..255] ]
bCrTab = V.fromListN 256 [(- fix 0.08131) * i | i <- [0..255] ]


instance ColorSpaceConvertible PixelRGB8 PixelYCbCr8 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelRGB8 r g b) = PixelYCbCr8 (fromIntegral y) (fromIntegral cb) (fromIntegral cr)
      where ri = fromIntegral r
            gi = fromIntegral g
            bi = fromIntegral b

            y  = (rYTab `V.unsafeIndex` ri + gYTab `V.unsafeIndex` gi + bYTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits
            cb = (rCbTab `V.unsafeIndex` ri + gCbTab `V.unsafeIndex` gi + bCbTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits
            cr = (bCbTab `V.unsafeIndex` ri + gCrTab `V.unsafeIndex` gi + bCrTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits

    convertImage Image { imageWidth = w, imageHeight = h, imageData = d } = Image w h newData
        where maxi = w * h

              rY  = fix 0.29900
              gY  = fix 0.58700
              bY  = fix 0.11400
              rCb = - fix 0.16874
              gCb = - fix 0.33126
              bCb = fix 0.5
              gCr = - fix 0.41869
              bCr = - fix 0.08131

              newData = runST $ do
                block <- M.new $ maxi * 3
                let traductor _ idx | idx >= maxi = return block
                    traductor readIdx idx = do
                        let ri = fromIntegral $ d `V.unsafeIndex` readIdx
                            gi = fromIntegral $ d `V.unsafeIndex` (readIdx + 1)
                            bi = fromIntegral $ d `V.unsafeIndex` (readIdx + 2)

                            y  = (rY * ri + gY * gi + bY * bi + oneHalf) `unsafeShiftR` scaleBits
                            cb = (rCb * ri + gCb * gi + bCb * bi + (128 `unsafeShiftL` scaleBits) + oneHalf - 1) `unsafeShiftR` scaleBits
                            cr = (bCb * ri + (128 `unsafeShiftL` scaleBits) + oneHalf - 1+ gCr * gi + bCr * bi) `unsafeShiftR` scaleBits

                        (block `M.unsafeWrite` (readIdx + 0)) $ fromIntegral y
                        (block `M.unsafeWrite` (readIdx + 1)) $ fromIntegral cb
                        (block `M.unsafeWrite` (readIdx + 2)) $ fromIntegral cr
                        traductor (readIdx + 3) (idx + 1)

                traductor 0 0 >>= V.freeze

crRTab, cbBTab, crGTab, cbGTab :: V.Vector Int
crRTab = V.fromListN 256 [(fix 1.40200 * x + oneHalf) `unsafeShiftR` scaleBits | x <- [-128 .. 127]]
cbBTab = V.fromListN 256 [(fix 1.77200 * x + oneHalf) `unsafeShiftR` scaleBits | x <- [-128 .. 127]]
crGTab = V.fromListN 256 [negate (fix 0.71414) * x | x <- [-128 .. 127]]
cbGTab = V.fromListN 256 [negate (fix 0.34414) * x + oneHalf | x <- [-128 .. 127]]

instance ColorSpaceConvertible PixelYCbCr8 PixelRGB8 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelYCbCr8 y cb cr) = PixelRGB8 (clampWord8 r) (clampWord8 g) (clampWord8 b)
        where clampWord8 = fromIntegral . max 0 . min 255
              yi = fromIntegral y
              cbi = fromIntegral cb
              cri = fromIntegral cr

              r = yi +  crRTab `V.unsafeIndex` cri
              g = yi + (cbGTab `V.unsafeIndex` cbi + crGTab `V.unsafeIndex` cri) `unsafeShiftR` scaleBits
              b = yi +  cbBTab `V.unsafeIndex` cbi

    convertImage Image { imageWidth = w, imageHeight = h, imageData = d } = Image w h newData
        where maxi = w * h
              clampWord8 v | v < 0 = 0
                           | v > 255 = 255
                           | otherwise = fromIntegral v

              newData = runST $ do
                block <- M.new $ maxi * 3
                let traductor _ idx | idx >= maxi = return block
                    traductor readIdx idx = do
                        let yi =  fromIntegral $ d `V.unsafeIndex` readIdx
                            cbi = fromIntegral $ d `V.unsafeIndex` (readIdx + 1)
                            cri = fromIntegral $ d `V.unsafeIndex` (readIdx + 2)

                            r = yi +  crRTab `V.unsafeIndex` cri
                            g = yi + (cbGTab `V.unsafeIndex` cbi + crGTab `V.unsafeIndex` cri) `unsafeShiftR` scaleBits
                            b = yi +  cbBTab `V.unsafeIndex` cbi

                        (block `M.unsafeWrite` (readIdx + 0)) $ clampWord8 r
                        (block `M.unsafeWrite` (readIdx + 1)) $ clampWord8 g
                        (block `M.unsafeWrite` (readIdx + 2)) $ clampWord8 b
                        traductor (readIdx + 3) (idx + 1)

                traductor 0 0 >>= V.freeze

instance ColorPlane PixelYCbCr8 PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane PixelYCbCr8 PlaneCb where
    toComponentIndex _ _ = 1

instance ColorPlane PixelYCbCr8 PlaneCr where
    toComponentIndex _ _ = 2
