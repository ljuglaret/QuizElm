module Main exposing (..)


import Bootstrap.Form as Form
import Html.Attributes exposing (style,src)
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Html exposing (Html,div,button,br)
import Html.Events exposing (onClick)
import Html exposing (text)
import Quiz exposing(Questionnaire,newQuestionnaire,lesErreurs,listOk, lesBonnesReponses)
import FonctionsAux exposing(factoriseReponsesDonnees)
import Browser 
import Bootstrap.Grid.Col as Col


import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN

import Bootstrap.Text exposing(..)




exemple : Questionnaire
exemple = newQuestionnaire
            [((1,"racine carre de 9 "),[((1,"-3"),True),((2,"4"),False),((3,"3"),True)]),
             ((2,"racine carre de 4"),[((1,"-4"),False),((2,"-2"),True),((3,"2"),True)]),
            ((3,"2*3"),[((1,"6"),True),((2,"5"),False)])
            ]

type Etat  = Initial | Corrige

type alias Model =  {
    questionnaire       :   Questionnaire,
    listeDesReponses    :   List (Int, Int, Bool),
    erreurs             : List String,
    score : Maybe Int ,
    reponsesFausses : Maybe (List (Int, Int)),
    etat : Etat
    }



init : Model
init =  {
        questionnaire       = exemple,
        listeDesReponses    =[],
        erreurs =   lesErreurs exemple,
       
        score   =   Nothing,
        reponsesFausses = Nothing,
         etat = Initial}




type Msg
  = 
    EffacerLeFormulaire    |  
    Change Int Int Bool | 
    GetScore


questionCochee : Model -> Int -> Int -> Bool 
questionCochee model idQuestion idReponse = 
                    modBy 2 (List.length (List.filter 
                        (\(idq,idr,_) -> 
                            (idq,idr)==(idQuestion,idReponse))
                    model.listeDesReponses)) /= 0


nbDeFoisQuestionCochee : Model -> Int -> Int
                        -> List(Int, Int,Bool)
nbDeFoisQuestionCochee model idQuestion idReponse = 
    (idQuestion ,
    idReponse   ,
    not(questionCochee model idQuestion idReponse))::model.listeDesReponses
      

correction : Model -> String  -> Int ->  String 
correction model intituleReponse idQ  =
    let
        repFausses : Maybe(List(Int,Int))
        repFausses = model.reponsesFausses

        listeDesReponsesFausses =
            listOk model.questionnaire
                |> List.concatMap Tuple.second 
                |> List.filter(\(_,b) -> not b)
                |> List.map Tuple.first
                |> List.map Tuple.second
        corresp = correspondance model.questionnaire idQ intituleReponse 

    in
        case repFausses of 
            Nothing -> intituleReponse
            Just repF ->
                if model.etat == Initial
                then intituleReponse
                else 
                    if  questionCochee model idQ corresp                        
                    then 
                        if List.any(\str -> str == intituleReponse) listeDesReponsesFausses
                           then intituleReponse ++" :   Faux"
                        else intituleReponse ++"    :   Vrai"
                    else intituleReponse

correspondance q idq nomr = 
    let
        lesQuestions = List.filter(\((id,_),l) -> id == idq)  (listOk q ) 
        lesReponses =  List.concatMap(\(_, l) -> List.filter(\((_,nom),_) -> nomr == nom) l ) lesQuestions
    in lesReponses |> List.map Tuple.first |> List.map Tuple.first |> List.head |> Maybe.withDefault 0

fabriqueQuestionnaire  : Model   -> Html Msg
fabriqueQuestionnaire model  =
    let
        questionsReponses : List ((Int,String), List ((Int, String),Bool))
        questionsReponses =  listOk model.questionnaire
        uneReponse (iDQuestion,nomIDRep) reponsesAssocieesALaQuestion = 
            Form.row []
            [
             Form.col  [ ]
                    [
                     Html.img[src ("questions/"++String.fromInt iDQuestion ++ ".png")][]
                    , Fieldset.config
                    |> Fieldset.children
                        (List.map(\(idRep, nomRep) -> 
                                    Checkbox.checkbox
                                    [  Checkbox.id nomIDRep,
                                       
                                        Checkbox.checked (questionCochee model iDQuestion idRep  ),
                                        Checkbox.onCheck(\cocheOuPas ->
                                                            Change iDQuestion idRep cocheOuPas),
                                        if  model.etat == Initial
                                        then Checkbox.disabled False
                                        else Checkbox.disabled True,

                                        if model.etat == Corrige && String.contains "Faux" (correction model nomRep iDQuestion )
                                        then Checkbox.danger
                                        else 
                                            if model.etat == Corrige && String.contains "Vrai" (correction model nomRep iDQuestion )
                                            then Checkbox.success
                                            else Checkbox.attrs[]
                                             
                                    ]
                                    (correction model nomRep iDQuestion ))
                                    reponsesAssocieesALaQuestion)
                                        
                    |> Fieldset.view
                    ]
                ] 
        in 
            questionsReponses|>
                List.map
                    (\(idEtIntituleQuestion,listeReponsesEtNom)->
                        uneReponse
                            idEtIntituleQuestion
                            (List.map
                                (\((id,nom),_) -> (id,nom))    
                                listeReponsesEtNom)
                    )
        |>Form.form[]

{-
((Int,String),[(Int)])
-}

fctScore  : Model -> Maybe (Int, List (Int,Int))
fctScore model =
    let
        score = List.foldr(\(idQ,idR,b) (accScore,accErreurs) ->
                                if List.member (idQ,idR,b) (lesBonnesReponses  model.questionnaire)
                                then (accScore + 1,accErreurs)
                                else (accScore - 1, (idQ,idR)::accErreurs  )) (0,[]) model.listeDesReponses
    in
        if List.isEmpty model.erreurs 
        then Just score
        else Nothing

    
update : Msg -> Model -> Model
update msg model = 
    case msg of
        EffacerLeFormulaire -> 
            init
        
        Change numQuestion numReponse _ ->
            {model | listeDesReponses =
                factoriseReponsesDonnees (nbDeFoisQuestionCochee model numQuestion numReponse)}   
        
        GetScore    ->  
            {model |score = 
                case fctScore model of 
                    Just x -> Just (Tuple.first  x )
                    _ -> Nothing
                    ,etat = Corrige
                    ,reponsesFausses =  case fctScore model of 
                        Just x -> Just(Tuple.second  x) 
                        _ -> Nothing
                    }
     

{-
[(1,1,True), (1,2,False), (1,3,True)]
-}
view : Model -> Html Msg
view model =
 Grid.container[]         -- Responsive fixed width container
        [ CDN.stylesheet      -- Inlined Bootstrap CSS for use with reactor
        ,if model.erreurs /= []
        then   div[](List.map text model.erreurs)
        else 
            fabriqueQuestionnaire model
            ,text (case model.score of 
                        Just s -> String.fromInt s
                        _ -> ""
                            )
            ,br[][]
            ,button [ onClick EffacerLeFormulaire  ] [ text "Efface le formulaire" ]
            ,br[][]
            ,button [ onClick GetScore ] [ text "score" ]
            

        ]
    
        
main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }








