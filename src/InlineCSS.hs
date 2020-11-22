{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module InlineCSS where

import Text.RawString.QQ (r)

baseCSS :: String
baseCSS =
  [r|
@font-face {
    font-family: OstrichSans;
    src: url("resources/OstrichSans-Medium.otf") format("opentype");
}
@font-face {
    font-family: OstrichSans;
    font-weight: bold;
    src: url("resources/OstrichSans-Heavy.otf") format("opentype");
}
@font-face {
    font-family: Monoid;
    src: url("resources/monoid-bold.woff") format("woff");
}
@font-face {
    font-family: Monoid;
    font-weight: bold;
    src: url("resources/monoid-bold.woff") format("woff");;
}
section.has-light-background, section.has-light-background h1, section.has-light-background h2, section.has-light-background h3, section.has-light-background h4, section.has-light-background h5, section.has-light-background h6 {
    color:  rgb(13, 42, 53);}

.reveal {
    font-family: "OstrichSans", Helvetica, sans-serif;
    font-size: 4em;
    font-weight: 700;
    background: rgb(13, 42, 53);
}

.reveal * {
    font-family: "OstrichSans", Helvetica, sans-serif;
    
}

.reveal strong {
    color: rgb(223,142,200);
}

.reveal em {
    font-weight: 100;
    font-style: normal;
}

pre.sourceCode {
    width: 100%;
}

code span.st {
    color: #268bd2;
    font-family: "Monoid", monospace !important;
    font-size: 0.9rem;
}

code span.kw {
    color: #007020;
    font-weight: bold;
    font-family: "Monoid", monospace !important;
}

code.sourceCode > span {
    font-family: "Monoid", monospace !important;
    font-size: 0.9rem;
}

code span.im {
    color: #cb4b16;
    font-family: "Monoid", monospace !important;
    font-size: 0.9rem;
}

.reveal h1, .reveal h2, .reveal h3, .reveal h4, .reveal h5 {
    text-transform: uppercase;
    font-family: "OstrichSans", Helvetica, sans-serif;
}

.reveal ul {
    width: 100%;
    text-align:center;
    padding:0;
    margin:0;
}

.reveal ul li {
    transform: scaleX(0.7);
        list-style:none;
        margin-bottom:6px;
        text-transform: uppercase;
}

ul > li:before {
    color: rgb(223,142,200);
    content: '> ';
}

.reveal .slide {
    height: 100% !important;
    top: auto;
    margin-top: auto !important;
    display: table;
}

.reveal .slide>section {
    min-height: 90% !important;
    top: auto;
}

.reveal .slide>section>section {
    min-height: 100% !important;
}

.left-float {
    display: table;
    width: 50%;
    float: left;
    height: 95%;
}

.overlay {
    background-blend-mode: darken;
    background-color: rgba(40, 40, 40, 0.6);
}

.background-multi-image {
    height: 95%;
    display: inline-block;
    background-position: calc(50%);
    background-repeat: no-repeat;
    background-size: cover;
    filter: blur(1px);
    -webkit-filter: blur(1px);
}

.two-image {
    width: 49%;
}

.three-image {
    width: 30%;
}

.four-image {
    width: 24%;
}

.five-image {
    width: 19%;
}


.fit-image {
    width: 100%;
    height: 95%;
    display: inline-block;
    background-position: calc(50%);
    background-repeat: no-repeat;
    background-size: contain;
}

.left {
    float: left;
}

.right {
    float: right;
    clear: both;
}

.centered-float {
    text-align: center;
    z-index: 20;
    position: absolute !important;
    top: calc(5%) !important;
    left: 0 !important;
    right: 0 !important;
    margin: 0 0;
}

.half-image-cover {
    width: 50%;
    height: 95%;
    display: inline-block;
    background-position: calc(50%);
    background-repeat: no-repeat;
    background-size: cover;
}

sup.superscript {
    font-size: 50% !important;
    color: #42affa;
    margin-left: -1rem;
}

.footnote {
    font-size: 30% !important;
    margin-top: calc(90%);
    display: inline-block;
}

.footnote-count {
    font-size: 40% !important;
    color: #42affa;
    margin-top: calc(90%);
    display: inline-block;
}

.half-image-fit {
    width: 50%;
    height: 95%;
    display: inline-block;
    background-position: calc(50%);
    background-repeat: no-repeat;
    background-size: contain;
}

.container{
    display: flex;
    height: 100%;
    align-items: center;
    justify-items: center;
    flex-direction: row;
}

.col{
    display: block;
    flex-grow: 1;  
    align-items: center;
    justify-items: center;
}

.row {
    display: flex !important;
    align-items: center;
    justify-items: center;
    flex-grow: 1;
    flex-direction: column;  
}
|]
