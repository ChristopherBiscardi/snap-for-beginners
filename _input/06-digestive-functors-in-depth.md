## More Samples

The code for these samples is in `code/df-samples`. The app in df-samples is a copy of the Twitter app, with various forms, routes and templates added.

## Adding Splices

### digestiveSplices

`digestiveSplices` are the splices that comes from `digestive-functors-heist`. It takes a View T.Text (for example the one in `tweetFormHandler` in `code/df-one/src/Twitter.hs`)

### bindDigestiveSplices

`bindDigestiveSplices` is used very similar to heist's `bindSplices`. As seen in `code/df-one/src/Twitter.hs` from earlier in this chapter:

``` {.haskell .numberLines startFrom="38"}
tweetFormHandler :: Handler App App ()
tweetFormHandler = do
  (view, result) <- runForm "tweet" tweetForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "tweetform"
```

### digestiveSplices and custom splices

`bindDigestiveSplices` returns a `heistState`, just like `bindSplices` does. This means that if we need to add custom splices *and* `digestiveSplices` to render a template, we can do it like this:

Provided we have these imports:

```haskell
import qualified Heist.Interpreted as I
import Snap.Snaplets.Heist
```

we can change this from `code/df-one/src/Twitter.hs`

```haskell
heistLocal (bindDigestiveSplices view) $ render "tweetform"
```

to this

```haskell
heistLocal bSplices $ render "tweetform"
  where mysplices = [("thing", I.textSplice "what")
                    ,("otherthing", I.textSplice "other")]
        bSplices = (I.bindSplices mysplices . bindDigestiveSplices view)
```

The key part is replacing `bindDigestiveSplices view` with:

```haskell
I.bindSplices mysplices . bindDigestiveSplices view
```

where `mysplices` is a list of simple `textSplice`s.

## Forms

All of these return `Formlet`s that can be used when creating forms.

### text

`text` defines a text form and takes an optional default value (`Just "some text"` or `Nothing`)

```haskell
"username" .: text Nothing
```
and as seen in `code/df-one/src/Twitter.hs`:
```haskell
"username" .: check userErrMsg isNotEmpty (text Nothing)
```

### string

Same as `text`, but works on the `String` type.

### stringRead

`stringRead` is used for parseable and serializable values. In our Tweet example, we used `Int`. We can check the documentation for [Int][int] to see that in the `Instances` list there are instances for `Read Int` and `Show Int`. `Read` and `Show` are the instances that make a value "parseable and serializable". 

It takes an error message and possibly a default value (either `Just 5` or `Nothing` in our case).

```haskell
"timestamp" .: stringRead "My Error Message" Nothing
```

### choice

A `choice` form can be used to restrict options, such as with a select tag. It takes a list of value, identifier pairs and an optional default value. From `code/df-samples/Twitter.hs`:

```haskell
-- Data Type with two possible values.
data Thing = ThingOne | ThingTwo
           deriving (Show, Eq)

-- Form for data type using choice. "t1" and "t2" 
--  will render in the dfInputSelect box
thingForm :: (Monad m) => Form T.Text m Thing
thingForm = "thething"  .: choice [(ThingOne, "t1"),
                                   (ThingTwo, "t2")] Nothing
```

Showing the flexibility of Digestive Functors, we have two template choices set up to render our choices. The first is at `localhost:8000/thing`, which renders a select box, the second is at `localhost:8000/thingradio`, which renders a set of radio buttons. Here are the relevant parts of both templates:

```html
<dfLabel ref="thething">The Thing: </dfLabel>
<dfInputSelect ref="thething" />
```

and

```html
<dfLabel ref="thething">The Thing: </dfLabel>
<dfInputRadio ref="thething" />
```

### bool

`bool` is for Boolean values, like true and false and of course, takes an optional default value (Such as `Just True` or `Nothing`).

```haskell
data Runner = Runner {
  isRunner :: Bool,
  name :: T.Text
  } deriving (Show)

runnerForm :: (Monad m) => Form T.Text m Runner
runnerForm = Runner
  <$> "isrunner" .: bool Nothing
  <*> "name" .: text Nothing
```

and the relevant templates in which we use a checkbox:

```html
<dfLabel ref="name">Name: </dfLabel>
<dfInputText ref="name" />
<br>

<dfLabel ref="isrunner">Are you a Runner? </dfLabel>
<dfInputCheckbox ref="isrunner"/>
<br>
```

## Testing Optional Values

### optionalText

`optionalText` functions the same way as `text` with one difference; It accepts `Maybe` values (that is, it's for fields that don't need to be filled out).

```haskell
data MRunner = MRunner {
  isMRunner :: Bool,
  mName :: Maybe T.Text
  } deriving (Show)

mRunnerForm :: (Monad m) => Form T.Text m MRunner
mRunnerForm = MRunner
  <$> "isrunner" .: bool Nothing
  <*> "name" .: optionalText Nothing
```

This form can use the same template as a `text` field. For example, here's the template from the `bool` example which had a `text` name:

```html
<dfLabel ref="name">Name: </dfLabel>
<dfInputText ref="name" />
<br>

<dfLabel ref="isrunner">Are you a Runner? </dfLabel>
<dfInputCheckbox ref="isrunner"/>
<br>
```

Two potential successful form responses:
```JavaScript
MRunner {isMRunner = True, mName = Just "Chris"}
MRunner {isMRunner = True, mName = Nothing}
```

## Digestive Splices

The Digestive Splices are what are used to construct templates. If you are familiar with html input tags already, most of them are named the same way as the html tag they generate. Some examples are given here but a complete list can be found in the Heist [docs][heistdocs] on Hackage.

### dfInput

Generates an `<input>` tag with the desired type.

```html
<dfInput type="date" ref="date" />
```

### dfInputText

This is how most of the other input tags are used. Define a `ref` attribute so that we can access it in our form validation. This ref will be translated to a namespaced equivalent (Something like `myform.name` in this example).

```html
<dfInputText ref="username" />
```
becomes
```html
<input type="text" id="tweet.username" name="tweet.username" value="">
```

#### A More Complicated Example

```html
<dfInput ref="timestamp" type="number" min="0" step="1" pattern="\d+" />
```
becomes
```html
<input type="number" min="0" step="1"
 pattern="\d+" id="tweet.timestamp"
 name="tweet.timestamp" value="">
```

### Similar Splices

* dfInputTextArea
* dfInputPassword
* dfInputHidden
* dfInputSelect
* dfInputRadio
* dfInputCheckbox

### dfInputSubmit

```html
<dfInputSubmit value="Submit" />
```
becomes
```html
<input value="Submit" type="submit">
```

### dfLabel

```html
<dfLabel ref="timestamp">Timestamp: </dfLabel>
```
becomes
```html
<label for="tweet.timestamp">Timestamp: </label>
```

### dfForm

`dfForm` is the equivalent of a `<form>` tag.

```html
<dfForm action="/tweet">
```
becomes
```html
<form action="/tweet" method="POST" enctype="application/x-www-form-urlencoded">
```

### dfErrorList

`dfErrorList` renders the errors for a specific input on the form.

```html
<dfErrorList ref="name">
```

### dfChildErrorList

`dfChildErrorList` renders as a list of all of the errors in the form.

```html
<dfChildErrorList ref="" />
```
could render as
```html
<ul>
<li>Username can not be empty</li>
<li>timestamp must be an Int</li>
<li>Tweet can not be empty</li>
</ul>
```

[int]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#t:Int
[heistdocs]: http://hackage.haskell.org/package/digestive-functors-heist-0.8.4.1/docs/Text-Digestive-Heist.html#g:2