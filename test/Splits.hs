{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Splits where

import Data.Text (pack)
import Text.RawString.QQ

cases =
  [ (twoSplit, twoSplitSlide),
    (threeSplit, threeSplitSlide),
    (fourSplit, fourSplitSlide),
    (fiveSplit, fiveSplitSlide)
  ]

twoSplit =
  pack
    [r|![](images/1.png)
![](images/2.png)

# In the middle
|]

twoSplitSlide =
  pack
    [r|<div class='background-multi-image two-image overlay' style='background-image: url(images/1.png);'></div>
<div class='background-multi-image two-image overlay' style='background-image: url(images/2.png);'></div>
::: centered-float
## In the middle
:::

---
|]

threeSplit =
  pack
    [r|![](images/1.png)
![](images/2.png)
![](images/3.png)

# In the middle
|]

threeSplitSlide =
  pack
    [r|<div class='background-multi-image three-image overlay' style='background-image: url(images/1.png);'></div>
<div class='background-multi-image three-image overlay' style='background-image: url(images/2.png);'></div>
<div class='background-multi-image three-image overlay' style='background-image: url(images/3.png);'></div>
::: centered-float
## In the middle
:::

---
|]

fourSplit =
  pack
    [r|![](images/1.png)
![](images/2.png)
![](images/3.png)
![](images/4.png)

# In the middle
|]

fourSplitSlide =
  pack
    [r|<div class='background-multi-image four-image overlay' style='background-image: url(images/1.png);'></div>
<div class='background-multi-image four-image overlay' style='background-image: url(images/2.png);'></div>
<div class='background-multi-image four-image overlay' style='background-image: url(images/3.png);'></div>
<div class='background-multi-image four-image overlay' style='background-image: url(images/4.png);'></div>
::: centered-float
## In the middle
:::

---
|]

fiveSplit =
  pack
    [r|![](images/1.png)
![](images/2.png)
![](images/3.png)
![](images/4.png)
![](images/5.png)

# In the middle
|]

fiveSplitSlide =
  pack
    [r|<div class='background-multi-image five-image overlay' style='background-image: url(images/1.png);'></div>
<div class='background-multi-image five-image overlay' style='background-image: url(images/2.png);'></div>
<div class='background-multi-image five-image overlay' style='background-image: url(images/3.png);'></div>
<div class='background-multi-image five-image overlay' style='background-image: url(images/4.png);'></div>
<div class='background-multi-image five-image overlay' style='background-image: url(images/5.png);'></div>
::: centered-float
## In the middle
:::

---
|]
