-- Press a button to send a GET request for random quotes.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--
module Main exposing (..) 

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, id, type_, value, for)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, map2, map4, field, int, string)
import Random
import HTTP_Error exposing (errorToString)  
import Array exposing (Array)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type Status
    = Loading           -- Recherche d'un mot...
    | Playing           -- Le joueur devine
    | Won               -- Victoire
    | ErrorString String -- Problème technique

type alias Model =
    { allWords : Array String
    , currentWord : String
    , definitions : List String
    , userGuess : String
    , status : Status
    }


type Msg
    = GenerateNewWord
    | GotRandomWord String  -- <-- On reçoit directement le mot ! (Plus de Int)
    | GotDef (Result Http.Error (List String))
    | UpdateGuess String
    | CheckGuess


init : () -> (Model, Cmd Msg)
init _ =
    let  wordsArray = 
                      raw_wordList
                      |> String.words  -- Sépare les mots
                      |> Array.fromList
    in
      ( { allWords = wordsArray
          , currentWord = ""
          , definitions = []
          , userGuess = ""
          , status = Loading
          }
        , Random.generate GotRandomWord (wordGenerator wordsArray))




-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      GenerateNewWord ->
          ( { model | status = Loading, userGuess = "", definitions = [] }
                  , Random.generate GotRandomWord (wordGenerator model.allWords))

      GotRandomWord word ->
          ( { model | currentWord = word}
          , getDefinitions word
          )

      GotDef result ->
              case result of
                  Ok defs ->
                      if List.isEmpty defs then
                          -- Si le mot n'a pas de définition, on en cherche un autre tout de suite
                          ( model, Random.generate GotRandomWord (wordGenerator model.allWords) )
                      else
                          ( { model | definitions = defs, status = Playing }, Cmd.none )

                  Err error ->
                      ( { model | status = ErrorString (errorToString error) }, Cmd.none )
      UpdateGuess guess ->
          ( { model | userGuess = guess }, Cmd.none )

      CheckGuess ->
          if isGuessCorrect model.userGuess model.currentWord then
              ( { model | status = Won }, Cmd.none )
          else
              ( model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [ style "font-family" "sans-serif", style "max-width" "600px", style "margin" "40px auto", style "padding" "20px" ]
      [ h1 [ style "text-align" "center" ] [ text "GuessIt" ]
    , h2 [] [ text "Random Definition"]
    , viewContent model
    ]
  


viewContent : Model -> Html Msg
viewContent model =
  case model.status of


    Loading -> -- Affiche un message de chargement pendant que les définitions sont récupérées
      div [ style "text-align" "center", style "color" "#888" ] 
                [ text "Chargement d'un mot..." ]


    Playing ->
      div []
        [ p [] [ text "Guess the word based on its definitions:" ] -- Affiche les définitions (si elles existent), fonctionnement normal 
        , ul [] (List.map (\def -> li [style "margin-bottom" "5px"] [ text def ]) model.definitions) -- lambda fonction pour afficher chaque définition dans une liste
        , div [ style "margin-top" "20px", style "background" "#f9f9f9", style "padding" "15px", style "border-radius" "5px" ]
                    [ label [ for "guess-input", style "margin-right" "10px" ] [ text "Your guess:" ]
                    , input 
                        [ id "guess-input"
                        , type_ "text"
                        , value model.userGuess
                        , onInput UpdateGuess
                        , style "padding" "5px"
                        ] []
                    , button 
                        [ onClick CheckGuess
                        , style "margin-left" "10px", style "padding" "5px 10px", style "cursor" "pointer"
                        ] [ text "Check" ]
                    ]
        ]

    Won ->
      div [ style "text-align" "center", style "color" "green" ] -- Ajout de styles pour la victoire
        [ h2 [] [ text "Congratulations! You guessed the word!" ]
        , p [] [ text ("The word was: " ++ model.currentWord) ]
        , button 
            [ onClick GenerateNewWord
            , style "margin-top" "20px", style "padding" "10px 20px", style "font-size" "1.2em", style "cursor" "pointer"
            ] 
            [ text "Play Again!" ]
        ]

    ErrorString errorMessage ->
      div [ style "color" "red" ]
        [ p [ style "color" "red" ] [ text ("Error: " ++ errorMessage ++ " ") ] -- Affiche le message d'erreur
        , text "I could not load a random definition for some reason. "
        , button [ onClick GenerateNewWord ] [ text "Try Again!" ]
        ]




-- Functions 


-- Cette fonction prépare le tirage au sort directement sur les mots
wordGenerator : Array String -> Random.Generator String
wordGenerator words =
    let
        maxIndex = Array.length words - 1
    in
    -- On tire un chiffre, et on le transforme (map) immédiatement en mot
    Random.int 0 maxIndex
        |> Random.map (\index -> Maybe.withDefault "apple" (Array.get index words))


getDefinitions : String -> Cmd Msg
getDefinitions word =
    Http.get
        { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ word
        , expect = Http.expectJson GotDef decodeDefinitions
        }


decodeDefinitions : Decode.Decoder (List String)
decodeDefinitions =
    let
        -- Mettre la première lettre en majuscule pour une meilleure présentation
        capitalize : String -> String
        capitalize str =
            String.toUpper (String.left 1 str) ++ String.dropLeft 1 str

        -- Cette fonction combine le type (ex: "Noun") en majuscule et la définition
        formatDefinition partOfSpeech definitions =
            let
                formattedType = capitalize partOfSpeech
            in

            List.map (\def -> "(" ++ formattedType ++ ") " ++ def) definitions

        -- Décodeur pour un bloc de sens (formattedType + définitions)
        decodeMeaningAndFormat =
            Decode.map2 formatDefinition
                (Decode.field "partOfSpeech" Decode.string)
                (Decode.field "definitions" 
                    (Decode.list (Decode.field "definition" Decode.string))
                )
    in
    Decode.list 
        (Decode.field "meanings" 
            (Decode.list decodeMeaningAndFormat)
        )
        -- On a une structure imbriquée : Liste de Mots -> Liste de Sens -> Liste de Définitions
        -- On utilise List.concat deux fois pour tout aplatir en une seule grande liste de Strings
        |> Decode.map (List.concat >> List.concat)


isGuessCorrect : String -> String -> Bool
isGuessCorrect guess target =
    let
        cleanGuess = String.toLower (String.trim guess)
        cleanTarget = String.toLower (String.trim target)
    in
    cleanGuess == cleanTarget


raw_wordList : String
raw_wordList = 
    """
a anywhere below burn climb able apartment bend bus close about appear beneath business clothes above approach beside busy cloud accept area best 
but coat across arm better buy coffee act around between by cold actually arrive beyond call college add art big calm color admit as bird camera come afraid 
ask bit can company after asleep bite car completely afternoon at black card computer again attack block care confuse against attention blood careful consider
age aunt blow carefully continue ago avoid blue carry control agree away board case conversation ahead baby boat cat cool air back body catch cop alive bad bone 
cause corner all bag book ceiling count allow ball boot center counter almost bank bore certain country alone bar both certainly couple along barely bother chair 
course already bathroom bottle chance cover also be bottom change crazy although beach box check create always bear boy cheek creature among beat brain chest
 cross and beautiful branch child crowd angry because break choice cry animal become breast choose cup another bed breath church cut answer bedroom breathe 
 cigarette dad any beer bridge circle dance anybody before bright city dark anymore begin bring class darkness anyone behind brother clean daughter anything
  believe brown clear day anyway belong building clearly dead death except funny history law decide excite future hit lay deep expect game hold lead desk explain
   garden hole leaf despite expression gate home lean die extra gather hope learn different eye gently horse leave dinner
 face get hospital leg direction fact gift hot less dirt fade girl hotel let disappear fail give hour letter discover fall glance house lie distance familiar
  glass how life do family go however lift doctor far god huge light dog fast gold human like door father good hundred line doorway fear grab hurry lip down 
  feed grandfather hurt listen dozen feel grandmother husband little drag few grass I local draw field gray ice lock dream fight great idea long dress figure
   green if look drink fill ground ignore lose drive final group image lot driver finally grow imagine loud drop find guard immediately love dry fine guess
    important low during finger gun in lucky dust finish guy information lunch each fire hair inside machine ear first half instead main early fish hall 
    interest make earth fit hallway into man easily five hand it manage east fix hang itself many easy flash happen jacket map eat flat happy job mark edge 
    flight hard join marriage eff ort floor hardly joke marry egg flower hate jump matter eight fly have just may either follow he keep maybe else food head 
    key me empty foot hear kick mean end for heart kid meet engine force heat kill member enjoy forehead heavy kind memory enough forest hell kiss mention 
    enter forever hello kitchen message entire forget help knee metal especially form her knife middle even forward here knock might event four herself know
     mind ever free hey lady mine every fresh hi land minute everybody friend hide language mirror everyone from high large miss everything front hill last 
     moment everywhere full him later money exactly fun himself laugh month moon our quickly send smile more out quiet sense smoke morning outside quietly 
     serious snap most over quite seriously snow mostly own radio serve so mother page rain service soft mountain pain raise set softly mouth paint rather
      settle soldier move pair reach seven somebody movie pale read several somehow much palm ready sex someone music pants real shadow something must paper
       realize shake sometimes my parent really shape somewhere myself part reason share son name party receive sharp song narrow pass recognize she soon near
  past red sheet sorry nearly path refuse ship sort neck pause remain shirt soul need pay remember shoe sound neighbor people remind shoot south never
 perfect remove shop space new perhaps repeat short speak news personal reply should special next
 phone rest shoulder spend nice photo return shout spin night pick reveal shove spirit no picture rich show spot nobody piece ride shower spread nod pile 
 right shrug spring noise pink ring shut stage none place rise sick stair nor plan river side stand normal plastic road sigh star north plate rock sight stare nose play roll sign start not please roof silence state note pocket room silent station nothing point round silver stay notice police row simple steal now pool rub simply step number poor run since stick nurse pop rush sing still of porch sad single stomach off position safe sir stone offer possible same sister stop office pour sand sit store officer power save situation storm often prepare say six story oh press scared size straight okay pretend scene skin strange old pretty school sky street on probably scream slam stretch once problem screen sleep strike one promise sea slide strong only prove search slightly student onto pull seat slip study open push second slow stuff or put see slowly stupid order question seem small such other quick sell smell suddenly suggest thick tree wash window suit thin trip watch wine summer thing trouble water wing sun think truck wave winter suppose third true way wipe sure thirty trust we wish surface this truth wear with surprise those try wedding within sweet though turn week without swing three twenty weight woman system throat twice well wonder table through two west wood take throw uncle wet wooden talk tie under what word tall time understand whatever work tea tiny unless wheel world teach tire until when worry teacher to up where would team today upon whether wrap tear together use which write television tomorrow usual while wrong tell tone usually whisper yard ten tongue very white yeah terrible tonight view who year than too village whole yell thank tooth visit whom yellow that top voice whose yes the toss wait why yet their touch wake wide you them toward walk wife young themselves town wall wild your then track want will yourself there train war win these travel warm wind"
"""