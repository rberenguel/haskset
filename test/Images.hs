{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Images where

import Data.Text (pack)
import Text.RawString.QQ

cases =
  [ (rightImage, rightImageSlide),
    (leftImage, leftImageSlide),
    (backgroundImageFitWithText, backgroundImageFitWithTextSlide),
    (backgroundImageFit, backgroundImageFitSlide)
  ]

rightImage =
  pack
    [r|
![right](images/1.png)

This is on the left, non-title
and multi-line
|]

rightImageSlide =
  pack
    [r|<div class='half-image-cover right ' style="background-image: url(images/1.png);"></div>
::: container
::: row


This is on the left, non-title
and multi-line
:::
:::

---
|]

leftImage =
  pack
    [r|
![left](images/2.png)

# This is on the right!
|]

leftImageSlide =
  pack
    [r|<div class='half-image-cover left ' style="background-image: url(images/2.png);"></div>
::: container
::: row
:::
::: col
## This is on the right!
:::
:::

---
|]

backgroundImageFitWithText =
  pack
    [r|![fit](images/1.png)

# **This is on the middle**
|]

backgroundImageFitWithTextSlide =
  pack
    [r|<div class='fit-image fit' style="background-image: url(images/1.png);"></div>
::: container
::: row
::: col
## **This is on the middle**
:::
:::
:::

---
|]

backgroundImageFit =
  pack
    [r|![fit](images/1.png)

^ This just is
|]

backgroundImageFitSlide =
  pack
    [r|<div class='fit-image fit' style="background-image: url(images/1.png);"></div>
::: container
::: row
::: col
<aside class='notes'> This just is
</aside>
:::
:::
:::

---
|]
