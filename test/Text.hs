{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text where

import Data.Text (pack)
import Text.RawString.QQ

cases =
  [ (boldHeader, boldHeaderSlide),
    (emphHeader, emphHeaderSlide),
    (multiFormatHeader, multiFormatHeaderSlide),
    (multiFormatHeader2, multiFormatHeaderSlide2),
    (buildLists, buildListsSlide),
    (buildListsRight, buildListsRightSlide),
    (buildOrderedLists, buildOrderedListsSlide),
    (footnotes, footnotesSlide)
  ]

boldHeader =
  pack
    [r|# __This is colourful__
|]

boldHeaderSlide =
  pack
    [r|::: container
::: row
::: col
## __This is colourful__
:::
:::
:::

---
|]

emphHeader =
  pack
    [r|# _This is thin_
|]

emphHeaderSlide =
  pack
    [r|::: container
::: row
::: col
## _This is thin_
:::
:::
:::

---
|]

multiFormatHeader =
  pack
    [r|# __T__his is spec_i_al
|]

multiFormatHeaderSlide =
  pack
    [r|::: container
::: row
::: col
##  __T__ his is spec _i_ al
:::
:::
:::

---
|]

multiFormatHeader2 =
  pack
    [r|# Th__is__ is al_so_ speci_al_
|]

multiFormatHeaderSlide2 =
  pack
    [r|::: container
::: row
::: col
## Th __is__  is al _so_  speci _al_ 
:::
:::
:::

---
|]

buildLists =
  pack
    [r|[.build-lists: true]

- This
- List
- Builds
|]

buildListsSlide =
  pack
    [r|


::: incremental
- This
- List
- Builds
:::

---
|]

buildListsRight =
  pack
    [r|[.build-lists: true]
> Here's a quote
-- This is not a list item, but an attribution

|]

buildListsRightSlide =
  pack
    [r|

> Here's a quote
-- This is not a list item, but an attribution


---
|]

buildOrderedLists =
  pack
    [r|[.build-lists: true]

1. This
1. List
2. Builds
|]

buildOrderedListsSlide =
  pack
    [r|


::: incremental
1. This
1. List
1. Builds
:::

---
|]

footnotes =
  pack
    [r|# This is a footnote [^1] in the middle

This is a foootnote too [^3]

But not at the end either

[^4]: And finally it goes here

|]

footnotesSlide =
  pack
    [r|::: container
::: row
::: col
## This is a footnote <sup class='superscript'>1</sup> in the middle

This is a foootnote too <sup class='superscript'>3</sup>

But not at the end either

<div class='footnote-count'>4 →</div><div class='footnote'>And finally it goes here

</div>
:::
:::
:::

---
|]
