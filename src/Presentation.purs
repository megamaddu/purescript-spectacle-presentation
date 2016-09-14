module App.Presentation where

-- | PureScript modules begin with imports.
-- | Notice they are very explicit and either import
-- | a specific thing by name or a group of things namespaced
-- | under an alias (like `import Pux.Html as H`).
-- | The compiler allows one "open" import (like `import Prelude`)
-- | which imports everything declared by that module into scope.
-- | Most modules reserve this for Prelude or Batteries (a larger Prelude)
-- | because they are used frequently and people begin to recognize imports
-- | from them quickly.  I left everything here explicit so there's no magic.
-- | If you want to know more about any of these imported modules,
-- | pursuit.purescript.org is a great place to start.
import Pux.Html as H
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple.Nested ((/\))
import Prelude (Unit, (<>))
import Pux.Html (Attribute, withAttr, withChild, withChildren, Html, span, i)
import Pux.Html.Attributes (size, className, target, style)
import Spectacle (deck, spectacle, slide, text, quote, blockQuote, listItem, layoutFit, layout, appear, list, cite, heading, link)
import Spectacle as S
import Spectacle.Attributes (padding, Progress(Bar), Transition(Slide), transitionDuration, transition, progress, preload, textColor, notes, bgColor, textSize, href, fit, margin, source, lang, theme)

-- | This is a view function.  There may be many,
-- | but in this app we only have one.  The name
-- | of the function is not important, but its
-- | type is!  We can see it takes no arguments
-- | and returns `Html Unit`.  Most Pux views
-- | will be functions from `s -> Html a`, where
-- | `s` is the type of your application state
-- | and `a` is the type of actions the view can
-- | produce.  This one uses `Unit` for the action
-- | type, the type used to represent nothing.
-- | It indicates this view produces no meaningful
-- | actions and any state is self-contained (In
-- | this case it's managed by Spectacle).
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
  -- | And that's the whole app!  Kinda.  Look at the
  -- | code above as JSX.  `<Spectacle theme={slideTheme} ... etc`.
  -- | `!` is a helper function which applies an attribute on the
  -- | right to an element on the left.  It's also chainable since
  -- | it returns elements.  `#` is a helper function for giving
  -- | an element a single child.  `##` takes an array of children.
  -- | These "operator" aliases are actually defined at the bottom
  -- | of this file, and you can define your own!  Don't go too
  -- | crazy though 😉
  where
    -- | This is the "where clause".  Values and functions can
    -- | defined here and are available both in the function body
    -- | above and in other definitions below.  Order does not
    -- | matter.  Where definitions are similar to module
    -- | definitions, but will not warn if you leave off the type
    -- | signature the way top-level definitions do.  In a larger
    -- | project many of these initial definitions would go in
    -- | config files or a Constants module of some kind.
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
    -- | Don't let the `/\` scare you.  It's just another function!
    -- | You can find it imported above on line 16 from the Tuple
    -- | module, and it's actually just an alias for the Tuple
    -- | constructor.  That means you can also write the line
    -- | above like this: `bg color = Tuple "backgroundColor" color`

    fontSize s = "fontSize" /\ s

    lightFont = "fontWeight" /\ "lighter"
    boldFont = "fontWeight" /\ "bold"

    slideTheme = theme
      { colors:
        { primary: stormy
        , secondary: stormy
        , tertiary: blue
        , quartenary: stormy
        }
      , fonts:
        { primary: "-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,sans-serif"
        , secondary: "-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,sans-serif"
        , tertiary: "Ahamono,Menlo,monospace"
        }
      }
    -- | These curly-brace structures are called Records.
    -- | At runtime they're just JavaScript objects, but
    -- | PureScript treats them much more strictly at
    -- | compile time.  They can't be mutated (by PureScript
    -- | code, so be careful giving them to other libraries
    -- | as arguments -- there are other types for representing
    -- | mutable JS data) and the compiler will track the
    -- | type of each field.

    assets = {}
    -- | I didn't end up using any pictures, so this went
    -- | unused.  You can put asset urls in here for
    -- | Spectacle to pre-load.

    link' = link ! style [ fg blue ] ! target "_blank"
    heading' = heading ! style [ lightFont ]
    -- | What's going on here!?  These elements have no
    -- | children!  PureScript functions are curried,
    -- | meaning each argument applied to a function
    -- | actually returns a new function that expects
    -- | the next argument.  This means these aren't
    -- | full `Html` elements yet and they can be
    -- | reused below with different children.

    titleHeading = heading' ! size 1
    titleSubHeading = heading' ! size 5
    titleText = text ! style [ lightFont ] ! textSize "20px"

    icon kind = i [ className ("fa fa-" <> kind) ] []
    -- | Here `i` is being passed two arrays, one of Attributes
    -- | and one of children.  This is the normal Pux element
    -- | API without the `!` and `#` helper functions.
    -- | You'll see both used interchangably, depending only
    -- | on what reads best in each situation.  PureScript
    -- | code is about expressiveness, backed by strong
    -- | type system support!
    -- | There's a new operator too, `<>`.  It's an alias for
    -- | `append`, and it takes any inputs which are "concatable".
    -- | In PureScript, most of these abstractions are named
    -- | after their similar concepts in abstract math, so `<>`
    -- | actually requires input types in the class Monoid.
    -- | It's a weird word, but you can substitute it for
    -- | "concatable" in your head for now.  Strings, Arrays,
    -- | and Lists are all Monoids, along with many others!
    -- | This stuff is explained really well in the book linked
    -- | to below, "Haskell Programming from first principles".

    sectionTitleSlide color titleTxt noteTxt =
      slide
        ! bgColor color
        ! notes noteTxt
        # heading' ! size 3 ! textColor white #> titleTxt
    -- | `sectionTitleSlide` is a helper function that builds
    -- | a whole slide (we haven't seen whole slides yet, but
    -- | there are lots more below)!  The slides were getting
    -- | repetitive and their purpose unclear.  Moving
    -- | the repetitive bit into a named function makes
    -- | the code safer and easier to understand at a glance!
    -- | This particular helper builds the colorful title
    -- | slides between each section.

    contentSlide noteTxt =
      slide
        ! bgColor white
        ! notes noteTxt

    codePane l src =
      S.codePane
        ! textSize "2.2rem"
        ! margin "0 auto"
        ! bgColor white
        ! lang l
        ! source src
        ## []

    codePaneSmall l src =
      S.codePane
        ! textSize "1.4rem"
        ! margin "0 auto 0"
        ! padding "0 1em"
        ! bgColor white
        ! lang l
        ! source src
        ## []

    js = "javascript"
    ps = "haskell"

    resourceList resources =
      list ! style [ "listStyleType" /\ "none" ] ## resources

    resource title author url =
      listItem # link' ! href url # blockQuote #
        quote ##
          [ text ! textSize "2rem" ! textColor blue #> title
          , text ! textSize "1.2rem" ! textColor darkBlue #> author
          ]
    -- | `#>` is like `#`, but it takes a String instead of Html
    -- | and wraps it in Pux's `text` component.  I probably wouldn't
    -- | do that in most projects, but this is a content-heavy
    -- | app with lots of embedded strings.  In a large app I'd probably
    -- | have it require inputs of some custom type like ContentKey
    -- | and use that key to look up the translation.  Aren't types cool?

    slides =
      [ contentSlide """
        Hello! How is everyone?
        Let's talk about PureScript..
        """ ##
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
        <div>So what is PureScript?</div>
        <div>Typed, Pure, Functional language, targeting JavaScript (or more specifically, CommonJS)</div>
        <div>(and in the future that will likely be ES6 modules)</div>
        """ ##
          [ list ! style [ "listStyleType" /\ "none" ] ##
            let
              appearingItem color attrs children =
                appear # listItem # (text ! textSize "5rem" ! style [ fg color, lightFont ]) attrs children
            in
              [ appearingItem green #> "Typed"
              , appearingItem red #> "Pure"
              , appearingItem orange #> "Functional"
              , appearingItem purple ##
                let
                  tn = span ! style [ fg stormy, fontSize "2rem", boldFont ]
                in
                  [ tn #> "targets "
                  , span #> "JavaScript"
                  , tn # appear # span #> " (CommonJS)"
                  ]
                -- | `let` expressions!  These are kinda like `where`, but
                -- | a `let` actually returns a value.  You say `let`,
                -- | declare some variables, then finish it with an `in`,
                -- | the scope where those variables are visible.  The `let`
                -- | returns the value the `in` body resolves to.  `let`s
                -- | are useful for reducing duplication or re-evaluation
                -- | in an isolated scope.  `where`s are more declarative.
              ]
          ]

      , contentSlide """
        <div>To compare, PureScript is..</div>
        <div>..lighter weight than Elm (no runtime) and more flexible</div>
        <div>..more functional in style than TypeScript, but otherwise very similar</div>
        <div>..similar to Haskell, but lighter (no runtime) and has less type system features</div>
        """ ##
          [ heading' ! size 3 #> "For comparison, PureScript is.."
          , list ! style [ "listStyleType" /\ "none" ] ##
            [ appear # listItem ! margin "1rem 0" #> "..lighter weight than Elm (no runtime) and more flexible"
            , appear # listItem ! margin "1rem 0" #> "..more functional in style than TypeScript, but otherwise very similar"
            , appear # listItem ! margin "1rem 0" #> "..similar to Haskell, but lighter (no runtime) and has less type system features"
            ]
          ]

      , sectionTitleSlide green "Typed" """
        <div>So what do I mean by "Typed"?</div>
        <div># drink #</div>
        """

      , contentSlide """
        <div>...read slide...</div>
        <div>These are types for the programmer, not the computer.</div>
        <div>Contrast with JavaScript types ('cause it has them!):</div>
        <div>Number, String, Boolean, Object</div>
        <div>If the types are there anyway, we should be explicit and make them do more work for us!</div>
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
        <div>Alright, I also said it was "Pure". What does that mean?</div>
        """

      , contentSlide """
        <div>Talking about purity in the context of "functional programming"</div>
        <div>consider this JS function.. is `average` pure?</div>
        <div>Yep!</div>
        """ ##
          [ codePane js """
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
        <div>Ok.. how about now? (stealing from Jamison!)</div>
        <div># drink #</div>
        <div>nope.. JS does nothing to help us here</div>
        <div>`average2` looks the same as `average` from the outside..</div>
        """ ##
          [ codePane js """
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
        <div>PureScript is "pure" because it uses types to represent side effects (intent).</div>
        <div>Effects like: localStorage, ajax, global/shared state, randomness, time, ...</div>
        <div>How does PureScript represent effects?</div>
        <div>Eff</div>
        """ ##
          [ heading' ! size 4 ! fit #> """PureScript is "pure" because it uses types to represent side effects"""
          , heading' ! size 4 ! fit ##
            [ appear # span #> "localStorage"
            , appear # span #> ", ajax"
            , appear # span #> ", shared state"
            , appear # span #> ", randomness"
            , appear # span #> ", time..."
            ]
          , appear # codePane ps "Eff (EffectRow) ResultType"
          ]

      , contentSlide """
        <div>Given that.. what do these functions do?</div>
        <div>read window.location</div>
        <div>Date.now</div>
        <div>Unit?</div>
        <div>ajax!</div>
        """ ##
          [ appear # codePane ps "foo :: Eff (dom :: DOM) Location"
          , appear # codePane ps "bar :: Eff (now :: NOW) Instant"
          , appear # codePane ps "baz :: Element -> Eff (dom :: DOM) Unit"
          , appear # codePane ps "qux :: UserId -> Aff (ajax :: AJAX) User"
          ]

      , sectionTitleSlide orange "Functional" """
        <div>Finally, I said PureScript was functional..</div>
        """

      , contentSlide """
        <div>What's that mean?</div>
        <div>"Functional" is very broad: JavaScript, ClojureScript, Reason, Elm, ...</div>
        <div>Most important concept: separation of data & functions</div>
        <div>In other words.. no Classes (in the JS/Java/C# sense)!</div>
        <div>Prefer pure/stateless functions where possible</div>
        <div>composition (currying)</div>
        """ ##
          [ heading' ! size 4 #> """...but "Functional" is very broad..."""
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
        <div>Some quick code examples...</div>
        <div># drink #</div>
        """

      , contentSlide """
        <div>here's average again -- ignore implementation, note the type -- no effects allowed!</div>
        <div>types can also change behavior -- note when using Int (+) changes</div>
        <div>classes (of types)!  not to be confused with JavaScript/Java/C# classes</div>
        """ ##
          [ list ! style [ "listStyleType" /\ "none" ] ##
            [ appear # listItem # codePaneSmall ps """
average :: Array Number -> Number
average nums = foldl (+) 0.0 nums / toNumber (length nums)
              """
            , appear # listItem # codePaneSmall ps """
average :: Array Int -> Int
average nums = foldl (+) 0 nums / length nums
              """
            ]
          ]

      , contentSlide """
        <div>ok, so far only built in types -- how do we make "types for the developer"?</div>
        <div>type alias -- records</div>
        <div>newtype wrappers</div>
        """ ##
          [ list ! style [ "listStyleType" /\ "none" ] ##
            [ appear # listItem # codePaneSmall ps """
type User = { email :: String }
              """
            , appear # listItem # codePaneSmall ps """
newtype Email = Email String
              """
            , appear # listItem # codePaneSmall ps """
someEmail :: Email
someEmail = Email "react@rally.com"
              """
            , appear # listItem # codePaneSmall ps """
type User = { email :: Email }
              """
            ]
          ]

      , contentSlide """
        <div>validation</div>
        <div>hiding type constructors</div>
        """ ##
          [ list ! style [ "listStyleType" /\ "none" ] ##
            [ appear # listItem # codePaneSmall ps """
parseEmail :: String -> Either String Email
parseEmail emailString =
  if test emailRegex emailString
    then Right (Email emailString)
    else Left "Invalid email address."
              """
            ]
          ]

      , contentSlide """
        <div>data types are like enums, but with super powers</div>
        """ ##
          [ list ! style [ "listStyleType" /\ "none" ] ##
            [ appear # listItem # codePaneSmall ps """
data AccountType = Admin | Employee | Customer
              """
            , appear # listItem # codePaneSmall ps """
data User
  = Anonymous
  | Guest { email :: Maybe Email }
  | FullAccount { email :: Email, name :: String }
              """
            , appear # listItem # codePaneSmall ps """
showLoginStatus :: User -> String
showLoginStatus Anonymous =
  "Login here!"
showLoginStatus (Guest { email }) =
  maybe "Subscribe!" (\_ -> "Create an account!") email
showLoginStatus (FullAccount { name }) =
  "Welcome back, " <> name <> "!"
              """
            ]
          ]

      , sectionTitleSlide pink "Pux" """
        <div>What about Pux?</div>
        <div># drink #</div>
        <div>"A PureScript interface to React."</div>
        <div>Quick disclaimer: probably PureScript's most active space</div>
        <div>purescript-react, Thermite, Halogen, Pux, & more..</div>
        <div>Pux is the simplest (slight safety tradeoffs too)</div>
        """

      , contentSlide """
        <div>Elm Architecture + PureScript + React = Pux</div>
        <div>But I'm not going to cover the Elm Architecture.. lots out there on that already</div>
        <div>Then what makes Pux unique?</div>
        """ ##
          [ codePaneSmall ps """
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
        <div>For those who don't know -- this is a slide with Spectacle</div>
        """ ##
          [ codePane js """
const View = () => (
  <Slide bgColor={white}>
    <CodePane source="...code..." />
  </Slide>
)
            """
          ]

      , contentSlide """
        <div>Then what makes Pux unique: React, particularly toReact & fromReact</div>
        <div>Here's a slide</div>
        """ ##
          [ codePane ps """
view =
  slide
    [ bgColor white ]
    [ codePane "...code..." ]
            """
          ]

      , contentSlide """
        <div>Root spectacle app in JS</div>
        """ ##
          [ codePaneSmall ps """
const View = () => (
  <Spectacle theme={theme}>
    <Deck
      progress="bar"
      transition={["slide", "slide"]}
      transitionDuration={500}
    >
      {slides}
    </Deck>
  </Spectacle>
)
            """
          ]

      , contentSlide """
        <div>And here's the root slideshow view</div>
        <div>notice ! and # instead of array pairs</div>
        """ ##
          [ codePaneSmall ps """
view =
  spectacle
    ! slideTheme
    ! preload assets
    # deck
      ! progress Bar
      ! transition [ Slide, Slide ]
      ! transitionDuration (Milliseconds 500.0)
      ## slides
            """
          ]

      , contentSlide """
        <div>how do we get these things into PureScript?</div>
        <div># drink #</div>
        <div>here's all the code for importing a React component into Pux</div>
        """ ##
          [ layout ##
            [ layoutFit # codePaneSmall ps """
-- | Spectacle.purs
foreign import codePane :: Component
              """
            , layoutFit
              ! style [ "borderLeft" /\ ("1px solid " <> stormy) ]
              # codePaneSmall js """
// Spectacle.js
var Pux = require("purescript-pux")
var Spectacle = require("spectacle")

exports.codePane =
  Pux.fromReact(Spectacle.CodePane)
              """
            ]
          ]

      , contentSlide """
        <div>what about using Pux components in a React app?</div>
        <div>here's normal app start</div>
        """ ##
          [ codePane ps """
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
        <div>here's an export for use in a JS app</div>
        <div>toReact</div>
        """ ##
          [ codePane ps """
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
        <div>Resources...</div>
        <div># drink #</div>
        """

      , contentSlide """
        <div>Learn PureScript</div>
        <div>Haskell Programming is ideal start -- it teaches you everything you need to know and most of the book applies to PureScript as well</div>
        <div>PureScript by Example -- language author's book, he keeps it up to date -- also very good but a little less introductory material -- a good followup to after the first few HaskellBook chapters</div>
        <div>From Callback to Future -- a great intoduction to concepts without the baggage of "scary" words or foreign syntaxes -- great read on "why would anyone want this"</div>
        <div>Alexis King's (or Lexi Lambda) blog has a lot of good content, but this post specifically addresses "how much abstraction is too much" -- a good read if you've been down this road before and found abstraction daunting -- take a little at a time</div>
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
        <div>Community...</div>
        """ ##
          [ heading' ! size 5 #> "Community"
          , resourceList
            [ resource "freenode.net #purescript" "IRC" "http://webchat.freenode.net/?channels=purescript"
            , resource "fpchat.com #purescript" "Slack" "http://fpchat.com"
            , resource "alexmingoia/purescript-pux" "Gitter" "https://gitter.im/alexmingoia/purescript-pux"
            ]
          ]

      , contentSlide """
        <div>This presentation...</div>
        """ ##
          [ heading' ! size 5 #> "This Presentation"
          , resourceList
            [ resource "Deployed Slideshow" "purescript-react-rally.surge.sh" "http://purescript-react-rally.surge.sh"
            , resource "Slideshow Code" "github.com/spicydonuts/purescript-spectacle-presentation" "https://github.com/spicydonuts/purescript-spectacle-presentation"
            , resource "purescript-pux-spectacle" "github.com/spicydonuts/purescript-pux-spectacle" "https://github.com/spicydonuts/purescript-spectacle"
            , resource "Pux Docs" "www.alexmingoia.com/purescript-pux/" "http://www.alexmingoia.com/purescript-pux/"
            ]
          ]

      , contentSlide """
        <div>About me...</div>
        <div># drink #</div>
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

-- | These helpers make the "html" a bit easier to work with.
withTextChild :: forall a. (Array (Attribute a) -> Array (Html a) -> Html a) -> String -> Html a
withTextChild comp txt = comp # H.text txt

infixl 1 withAttr as !
infixr 0 withChild as #
infixr 0 withTextChild as #>
infixr 0 withChildren as ##
