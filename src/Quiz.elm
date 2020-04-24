module Quiz exposing(..)

import List exposing(..)
import Tuple exposing(..)

import FonctionsAux exposing(sansDoublons)

type alias IdentifiantEtNom
    =   (Int,String)
type alias ReponsesPossibles
    =   (Int,String)
type alias VraiOuFaux
    =   Bool
type alias UneQuestionAlias
    =   (IdentifiantEtNom ,
        List(ReponsesPossibles, VraiOuFaux))
type UneQuestion
    =   C0 (Result String UneQuestionAlias)

type alias QuestionnaireAlias
    =   List UneQuestionAlias
type Questionnaire
    =   C2 (List UneQuestion)

type alias UneReponse
    =   (Int,Int,Bool)
type alias QuestionnaireTermineAlias =  List UneReponse

type QuestionnaireTermine = C  QuestionnaireTermineAlias

{-
les Bonnes Reponses  sont les couples d'entiers (idQuestion, idReponse)
    vrai dans un Questionnaire
-}
lesBonnesReponses : Questionnaire -> List(Int,Int,Bool)
lesBonnesReponses questionnaire =
    listOk questionnaire
    |> List.concatMap (\((id,_), lr)-> 
            List.foldr(\ ((idr,_),b) acc ->
                if b
                then (id,idr,b)::acc
                else acc)[] lr)

versUneQuestion : UneQuestion  -> Maybe UneQuestionAlias
versUneQuestion (C0 q) = Result.toMaybe q

newQuestion : UneQuestionAlias ->  UneQuestion
newQuestion questionAlias =
    let
        ((id,intitule), reponses) = questionAlias

        idstr : String
        idstr = String.fromInt id

        aucuneRepPossible :  Bool
        aucuneRepPossible = 
            List.map Tuple.second reponses
            |> List.all (\x -> x == False)
    in
        if List.isEmpty reponses 
        then C0(Err("pas de reponses question "++idstr))
        else
            if aucuneRepPossible
            then C0(Err("il doit y avoir au moins une reponse possibe" ++ idstr))
            else
                C0(Ok((id,intitule), sansDoublons reponses))

newQuestionnaire : QuestionnaireAlias -> Questionnaire
newQuestionnaire   questionnaire =
    let
        l  : List UneQuestion
        l  = List.map 
                newQuestion questionnaire
                 
        listeErreurs : List UneQuestion
        listeErreurs = List.filter(\(C0 x) -> Result.toMaybe x == Nothing ) l

        listeOk : List UneQuestion
        listeOk = List.filter(\(C0 x) -> Result.toMaybe x /= Nothing ) l
    in
        if not (List.isEmpty listeErreurs)
        then C2 listeErreurs
        else C2 listeOk
    

exemple : Questionnaire
exemple = newQuestionnaire
            [((1,"racine carre de 9 "),[((1,"-3"),True),((2,"4"),False),((4,"3"),False)]),
             ((2,"racine carre de 4"),[((1,"-4"),False),((2,"-2"),True),((3,"2"),True)]),
            ((3,"2*3"),[((1,"6"),True),((2,"5"),False)])
            ]

newQuestionnaireTermine  :  
    QuestionnaireTermineAlias
        -> Questionnaire
        -> QuestionnaireTermine 
newQuestionnaireTermine  questionnaireTermine questionnaire= C questionnaireTermine

{-newQuestionnaireTermine  :  
    QuestionnaireTermineAlias
        -> Questionnaire
        -> QuestionnaireTermine 
newQuestionnaireTermine  questionnaireTermine questionnaire= 
    let
        reponsesFournies idQuestion = associe questionnaireTermine idQuestion
        idsDesQuestions    : List Identifiant
        idsDesQuestions   = questionnaireIds questionnaire
    in C(
        List.foldr(\ numeroQuestion acc -> 
            {identifiant = numeroQuestion , 
            reponses     =  sansDoublons (reponsesFournies numeroQuestion)}:: acc) [] idsDesQuestions
      )
-}
questionnaireIds : Questionnaire  -> List Int 
questionnaireIds (C2 questionnaire) =
    List.foldr
        (\(C0 resultQuestion) acc -> 
            case resultQuestion of 
                Err _ -> acc
                Ok ((id,_),_) -> id::acc)
        []
        questionnaire

metDansLordre : QuestionnaireTermineAlias 
                -> Questionnaire
                -> QuestionnaireTermineAlias
metDansLordre l q = 
    let
        l2 = questionnaireIds q
    in  List.concatMap(\id -> List.filter(\(i1,_,_) -> i1 == id ) l)    l2



lesErreurs : Questionnaire -> List String
lesErreurs (C2 liste) = List.map(\(C0 q) -> q) liste
                        |> List.foldr
                            (\erreurs messagesErreurs -> 
                                case erreurs of 
                                    Ok _ -> messagesErreurs
                                    Err str -> str:: messagesErreurs
                            )
                            []
listOk: Questionnaire -> List UneQuestionAlias
listOk (C2 liste) = List.map(\(C0 q) -> q) liste
                        |> List.foldr
                            (\erreurs messagesErreurs -> 
                                case erreurs of 
                                    Err _ -> messagesErreurs
                                    Ok str -> str:: messagesErreurs
                            )
                            []




--correspondance : Questionnaire -> Int -> String -> Int



