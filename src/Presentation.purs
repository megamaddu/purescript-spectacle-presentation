module App.Presentation where

import Pux.Html as H
import Data.Options ((:=))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple.Nested ((/\))
import Prelude (Unit, (<>))
import Pux.Html (Attribute, withAttr, withChild, withChildren, Html, span, i)
import Pux.Html.Attributes (size, className, target, style)
import Spectacle (deck, spectacle, slide, text, quote, blockQuote, listItem, layoutFit, layout, appear, list, cite, codePane, heading, link)
import Spectacle.Attributes (padding, Progress(Bar), Transition(Slide), transitionDuration, transition, progress, preload, textColor, notes, bgColor, textSize, href, fit, margin, source, lang, theme)

-- | These helpers make the "html" a bit easier to work with
withTextChild :: forall a. (Array (Attribute a) -> Array (Html a) -> Html a) -> String -> Html a
withTextChild comp txt = comp # H.text txt

infixl 1 withAttr as !
infixr 0 withChild as #
infixr 0 withTextChild as #>
infixr 0 withChildren as ##

view :: Html Unit
view =
  spectacle
    ! slideTheme
    ! preload assets
    # deck
      ! progress Bar
      ! transition [ Slide, Slide ]
      ! transitionDuration (Milliseconds 500.0)
      ## slides
  where
    white = "white"
    black = "black"
    pink = "#e91e63"
    red = "#f44336"
    purple = "#9c27b0"
    blue = "#03a9f4"
    darkBlue = "#2196f3"
    stormy = "#607d8b"
    green = "#4caf50"
    orange = "#ff9800"

    fg color = "color" /\ color
    bg color = "backgroundColor" /\ color

    fontSize s = "fontSize" /\ s

    lightFont = "fontWeight" /\ "lighter"
    boldFont = "fontWeight" /\ "bold"

    slideTheme = theme
      { colors:
        { primary: stormy
        , secondary: stormy
        , tertiary: blue
        , quartenary: stormy }
      , fonts:
        { primary: "-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,sans-serif"
        , secondary: "-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,sans-serif"
        , tertiary: "Ahamono,Menlo,monospace" } }

    assets =
      {}

    link' = link ! style [ fg blue ] ! target "_blank"
    heading' = heading ! style [ lightFont ]

    titleHeading = heading' ! size 1
    titleSubHeading = heading' ! size 5
    titleText = text ! style [ lightFont ] ! textSize "20px"

    icon kind = i [ className ("fa fa-" <> kind) ] []

    sectionTitleSlide color titleTxt noteTxt =
      slide
        ! bgColor color
        ! notes noteTxt
        # heading' ! size 3 ! textColor white #> titleTxt

    contentSlide noteTxt =
      slide
        ! bgColor white
        ! notes noteTxt

    codePane' lg src =
      codePane
        ! textSize "2.2rem"
        ! margin "0 auto"
        ! bgColor white
        ! lang lg
        ! source src
        ## []
    smallCodePane lg src =
      codePane
        ! textSize "1.4rem"
        ! margin "0 auto 0"
        ! padding "0 1em"
        ! bgColor white
        ! lang lg
        ! source src
        ## []
    psCode = codePane' "haskell"
    jsCode = codePane' "javascript"

    resourceList resources =
      list ! style [ "listStyleType" /\ "none" ] ## resources
    resource title author url =
      listItem # link' ! href url # blockQuote #
        quote ##
          [ text ! textSize "2rem" ! textColor blue #> title
          , text ! textSize "1.2rem" ! textColor darkBlue #> author
          ]

    slides =
      [ contentSlide "Hello!" ##
        [ titleHeading ##
          [ span ! style [ fg stormy ] #> "PureScript "
          , span ! style [ boldFont ] #> "&"
          , span ! style [ fg stormy ] #> " Pux"
          ]
        , titleSubHeading #> "a safer, more expressive way to React"
        , text ! margin "4rem 0"
          # link'
            ! href "http://purescript-react-rally.surge.sh"
            #> "purescript-react-rally.surge.sh"
        ]

      , contentSlide """

        """ ##
          [ list ##
            [ listItem # text ! textSize "5rem" ! style [ fg green ] #> "Typed"
            , listItem # text ! textSize "5rem" ! style [ fg red ] #> "Pure"
            , listItem # text ! textSize "5rem" ! style [ fg orange ] #> "Functional"
            , appear # listItem # text ! textSize "5rem" ##
              let
                tn = span ! style [ fg stormy, fontSize "2rem" ]
                tb = span ! style [ fg purple ]
              in
                [ tn #> "targets "
                , tb #> "JavaScript"
                , tn # appear # span #> " (CommonJS)"
                ]
            ]
          ]

      , sectionTitleSlide green "Typed" """
        So what do we mean by "Typed"
        """

      , contentSlide """
        <p>So what do we mean by "Typed"</p>
        <p>...read slide...</p>
        <p>These are types for the programmer, not the computer.</p>
        <p>Contrast with JavaScript types ('cause it has them!):</p>
        <p>Number, String, Boolean, Object</p>
        """ ##
          [ blockQuote ##
            [ quote
              ! textSize "2.5rem"
              ##
                let
                  tn t = span ! style [ fg stormy, lightFont ] #> t
                  tb t = span ! style [ fg pink, lightFont ] #> t
                in
                  [ text # tn "There are many ways of trying to understand programs. "
                  , appear # text ##
                    [ tb "People often rely too much on one way,"
                    , tn " which is called “"
                    , tb "debugging"
                    , tn "” and consists of "
                    , tb "running a partly-understood program to see if it does what you expected"
                    , tn ". "
                    ]
                  , appear # text ##
                    [ tb "Another way"
                    , tn ", which ML advocates, "
                    , tb "is to install some means of understanding in the very programs themselves"
                    , tn "."
                    ]
                  ]
            , appear # cite ! textSize "3rem" #> "Robin Milner"
            ]
          ]

      , sectionTitleSlide red "Pure" """
        <p>Alright, I also said it was "Pure". What does that mean?</p>
        """

      , contentSlide """
        <p>Talking about functional purity</p>
        <p>consider this JS function.. is `average` pure?</p>
        <p>Yep!</p>
        """ ##
          [ appear # jsCode """
function average (nums) {
  let total = 0
  nums.forEach((num) => {
    total += num
  })
  return total / nums.length
}
            """
          ]

      , contentSlide """
        <p>Ok.. how about now? (stealing from Jamison!)</p>
        <p>nope.. JS does nothing to help us here</p>
        <p>`average2` looks the same as `average` from the outside..</p>
        """ ##
          [ jsCode """
function average2 (nums) {
  let total = 0
  nums.forEach((num) => {
    fireZeMissiles()
    total += num
  })
  return total / nums.length
}
            """
          ]

      , contentSlide """
        <p>PureScript is "pure" because it uses types to represent side effects (intent).</p>
        <p>Effects like: localStorage, ajax, global/shared state, randomness, time, ...</p>
        <p>How does PureScript represent effects?</p>
        <p>Eff</p>
        """ ##
          [ heading' ! size 4 ! fit #> """PureScript is "pure" because it uses types to represent side effects"""
          , heading' ! size 4 ! fit ##
            [ appear # span #> "localStorage"
            , appear # span #> ", ajax"
            , appear # span #> ", shared state"
            , appear # span #> ", randomness"
            , appear # span #> ", time..."
            ]
          , appear # psCode "Eff (EffectRow) ResultType"
          ]

      , contentSlide """
        <p>Given that.. what do these functions do?</p>
        """ ##
          [ appear # psCode "foo :: Eff (dom :: DOM) Location"
          , appear # psCode "bar :: Eff (now :: NOW) Instant"
          , appear # psCode "baz :: Element -> Eff (dom :: DOM) Unit"
          , appear # psCode "qux :: UserId -> Aff (ajax :: AJAX) User"
          ]

      , sectionTitleSlide orange "Functional" """
        <p>Finally, I said PureScript was functional..</p>
        """

      , contentSlide """
        <p>What's that mean?</p>
        <p>"Functional" is very broad: JavaScript, ClojureScript, Reason, Elm, ...</p>
        <p>Most important concept: separation of data & functions</p>
        <p>In other words.. no Classes (in the JS/Java/C# sense)!</p>
        <p>Prefer pure/stateless functions where possible</p>
        <p>composition (currying)</p>
        """ ##
          [ heading' ! size 4 #> """"Functional" is very broad..."""
          , list ##
            [ appear # listItem #> "JavaScript"
            , appear # listItem #> "ClojureScript"
            , appear # listItem #> "Reason"
            , appear # listItem #> "Elm"
            , appear # listItem #> "...many many more"
            ]
          , appear
            # span
              ! style [ lightFont ]
              ##
                [ span #> "Most important concept: "
                , span ! style [ fg darkBlue ] #> "separation of data & functions"
                ]
          ]

      , sectionTitleSlide stormy "Code" """
        Some quick code examples...
        """

      , contentSlide """
        """ ##
          [ list ! style [ "listStyleType" /\ "none" ] ##
            [ appear # listItem # smallCodePane "haskell" """
average :: Array Number -> Number
average nums = foldl (+) 0.0 nums / toNumber (length nums)
              """
            , appear # listItem # smallCodePane "haskell" """
type User = { email :: String }
              """
            , appear # listItem # smallCodePane "haskell" """
newtype Email = Email String
              """
            , appear # listItem # smallCodePane "haskell" """
type User = { email :: Email }
              """
            , appear # listItem # smallCodePane "haskell" """
average :: Array Int -> Int
average nums = foldl (+) 0 nums / length nums
              """
            ]
          ]

      , sectionTitleSlide pink "Pux" """
        <p>What about Pux?</p>
        <p>"A PureScript interface to React."</p>
        <p>Quick disclaimer: probably PureScript's most active space</p>
        <p>purescript-react, Thermite, Halogen, Pux, & more..</p>
        <p>Pux is the simplest (slight safety tradeoffs too)</p>
        """

      , contentSlide """
        <p>Elm Architecture + PureScript + React = Pux</p>
        <p>But I'm not going to cover the Elm Architecture.. lots out there on that already</p>
        <p>Then what makes Pux unique?</p>
        """ ##
          [ smallCodePane "haskell" """
data Action = Increment | Decrement

type State = Int

update :: Action -> State -> State
update Increment count = count + 1
update Decrement count = count - 1

view :: State -> Html Action
view count =
  div
    []
    [ button [ onClick (const Increment) ] [ text "Increment" ]
    , span [] [ text (show count) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]
            """
          ]

      , contentSlide """
        <p>Then what makes Pux unique: React, particularly toReact & fromReact</p>
        """ ##
          [ psCode """
view =
  slide [ bgColor white ]
    [ codePane "...code..." ]
            """
          ]

      , contentSlide """
        <p>Code for importing a React component into Pux</p>
        """ ##
          [ layout ##
            [ layoutFit # smallCodePane "haskell" """
-- | Spectacle.purs
foreign import codePane :: Component
              """
            , layoutFit
              ! style [ "borderLeft" /\ ("1px solid " <> stormy) ]
              # smallCodePane "javascript" """
// Spectacle.js
var Pux = require("purescript-pux")
var Spectacle = require("spectacle")

exports.codePane =
  Pux.fromReact(Spectacle.CodePane)
              """
            ]
          ]

      , contentSlide """
        <p>toReact</p>
        <p>here's normal app start</p>
        """ ##
          [ psCode """
main = do
  app <- start
    { initialState: 0
    , update
    , view
    , inputs: []
    }

  renderToDOM "#app" app.html
            """]

      , contentSlide """
        <p>toReact</p>
        <p>here's an export for use in a JS app</p>
        """ ##
          [ psCode """
toJSComponent = do
  comp <- start
    { initialState: 0
    , update
    , view
    , inputs: []
    }

  toReact comp.html
            """]

      , sectionTitleSlide darkBlue "Resources" """
        Resources...
        """

      , contentSlide """
        <p>Learn PureScript</p>
        <p>Haskell Programming is ideal start -- it teaches you everything you need to know and most of the book applies to PureScript as well</p>
        <p>PureScript by Example -- language author's book, he keeps it up to date -- also very good but a little less introductory material -- a good followup to after the first few HaskellBook chapters</p>
        <p>From Callback to Future -- a great intoduction to concepts without the baggage of "scary" words or foreign syntaxes -- great read on "why would anyone want this"</p>
        <p>Alexis King's (or Lexi Lambda) blog has a lot of good content, but this post specifically addresses "how much abstraction is too much" -- a good read if you've been down this road before and found abstraction daunting -- take a little at a time</p>
        """ ##
          [ heading' ! size 5 #> "Learn PureScript"
          , resourceList
            [ resource "From Callback to Future -> Functor -> Monad" "Yassine Elouafi" "https://medium.com/@yelouafi/from-callback-to-future-functor-monad-6c86d9c16cb5#.e0yd8022n"
            , resource "Haskell Programming (haskellbook.com)" "Julie Moronuki & Christopher Allen" "http://haskellbook.com"
            , resource "PureScript by Example" "Phil Freeman (creator of PureScript)" "https://leanpub.com/purescript"
            , resource "Climbing the infinite ladder of abstraction" "Alexis King" "https://lexi-lambda.github.io/blog/2016/08/11/climbing-the-infinite-ladder-of-abstraction/"
            ]
          ]

      , contentSlide """
        Community...
        """ ##
          [ heading' ! size 5 #> "Community"
          , resourceList
            [ resource "freenode.net #purescript" "IRC" "http://webchat.freenode.net/?channels=purescript"
            , resource "fpchat.com #purescript" "Slack" "http://fpchat.com"
            , resource "alexmingoia/purescript-pux" "Gitter" "https://gitter.im/alexmingoia/purescript-pux"
            ]
          ]

      , contentSlide """
        This presentation...
        """ ##
          [ heading' ! size 5 #> "This Presentation"
          , resourceList
            [ resource "Deployed Slideshow" "purescript-react-rally.surge.sh" "http://purescript-react-rally.surge.sh"
            , resource "Slideshow Code" "github.com/spicydonuts/purescript-spectacle-presentation" "https://github.com/spicydonuts/purescript-spectacle-presentation"
            , resource "purescript-pux-spectacle" "github.com/spicydonuts/purescript-pux-spectacle" "https://github.com/spicydonuts/purescript-spectacle"
            ]
          ]

      , contentSlide """
        About me...
        """ ##
          [ heading' ! size 5 #> "Hi, my name is Michael Trotter"
          , resourceList
            [ listItem
              # link' ! href "https://twitter.com/t_spicydonuts"
                # blockQuote # quote ##
                  [ text ! textSize "2rem" ! textColor blue ##
                    [ icon "twitter"
                    , H.text " t_spicydonuts" ]
                  , text ! textSize "1.2rem" ! textColor darkBlue #> "Twitter"
                  ]
            , listItem
              # link' ! href "https://github.com/spicydonuts"
                # blockQuote # quote ##
                  [ text ! textSize "2rem" ! textColor blue ##
                    [ icon "github"
                    , H.text " spicydonuts" ]
                  , text ! textSize "1.2rem" ! textColor darkBlue #> "GitHub"
                  ]
            , resource "Jane.com" "We're hiring! 🎉" "https://jane.com/careers"
            ]
          ]

      , slide
        ! bgColor black
        ! notes "Thanks!"
        # heading' ! size 3 ! textColor white ##
          [ H.text "Thank You "
          , i ! className "purescript-icon" #> ""
          ]
      ]
