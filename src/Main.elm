module Main exposing (..)


import Bootstrap.Form as Form
import Html.Attributes exposing (style,src)
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
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
            ((3,"4^-2"),[((1,"-2"),False),((2,"2"),True), ((3,"16"),False)])
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
correction model intituleReponse idQT  =
    let
        repFausses : Maybe(List(Int,Int))
        repFausses = model.reponsesFausses
        listeDesReponsesFausses  =
            List.foldr(\(idQ,idR,b) accErreurs ->
                                if List.member (idQ,idR,b)  (lesBonnesReponses  model.questionnaire)
                                then accErreurs
                                else  (idQ,idR)::accErreurs  ) [] model.listeDesReponses

        corresp = correspondance model.questionnaire idQT intituleReponse 

    in
        case repFausses of 
            Nothing -> intituleReponse
            Just repF ->
                if model.etat == Initial
                then intituleReponse
                else 
                    if  questionCochee model idQT corresp                        
                    then 
                        if List.member(idQT,corresp) listeDesReponsesFausses
                           then intituleReponse ++" :   Faux" {-Explication-}
                        else intituleReponse ++"    :   Vrai"
                    else intituleReponse{-++ "    :   Explication"-}

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
                     Html.img[style"scale" "1.2",src ("questions/"++String.fromInt iDQuestion ++ ".png")] []
                  
                    , Fieldset.config
                    |> Fieldset.children
                        (CDN.stylesheet::(   List.map(\(idRep, nomRep) -> 
                                    Checkbox.advancedCheckbox
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
                                            if  model.etat == Corrige && String.contains "Vrai" (correction model nomRep iDQuestion )
                                            then Checkbox.success
                                            else Checkbox.attrs[style "font-size" "120px"]
                                             
                                    ]
                                    (Checkbox.label [ style "font-size" "25px" ,style "font-family" "cursive"] [text(correction model nomRep iDQuestion)]))
                                    reponsesAssocieesALaQuestion))
                                        
                    
                    |> Fieldset.view, CDN.stylesheet
                    ]
                ] 
        in 
            Grid.container[]
                (questionsReponses|>
                    List.map
                        (\(idEtIntituleQuestion,listeReponsesEtNom)->
                            uneReponse
                                idEtIntituleQuestion
                                (List.map
                                    (\((id,nom),_) -> (id,nom))    
                                    listeReponsesEtNom)
                        )
                )
        
        
       

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
        [ 
        
        Grid.row[][
            Grid.col[][
            CDN.stylesheet      -- Inlined Bootstrap CSS for use with reactor
            ,if model.erreurs /= []
            then   div[](List.map text model.erreurs)
            else 
                fabriqueQuestionnaire model
                ,Html.div[style "width" "100%"][
                    Html.div[style "float" "left"][Html.button (onClick GetScore:: styleBoutons) [ text "Score" ]]
                
                ,   Html.div[style "margin-right" "85%" ,style "float" "right", style "font-family" "cursive",  style "font-size" "25px", style "color" "brown"]
                            [text (case model.score of
                                    Just s -> String.fromInt s
                                    _ -> ""
                                )]
                ]
            ]
        ]
        ,Grid.row [][Grid.col[][ Html.button (onClick EffacerLeFormulaire ::styleBoutons) [ text "Recommencer" ]]]
        ]
    --
    
        
main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

styleBoutons : List (Html.Attribute msg)
styleBoutons =  [ style "background-color" "purple", style "font-family" "cursive",  style "font-size" "25px", style "color" "white", style "text" "center "] 
 






