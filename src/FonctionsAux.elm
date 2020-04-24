module FonctionsAux exposing(..)

occurences : c -> List (c,a,b) -> Int
occurences element liste =
    List.foldr (\(id,_,_) occurencesId ->
                if id == element
                then occurencesId + 1
                else occurencesId ) 0 liste



sansDoublons : List a -> List a
sansDoublons l = List.foldr (\eltL listeSansDoublons -> 
                                if List.member eltL listeSansDoublons
                                then listeSansDoublons
                                else eltL :: listeSansDoublons
                                ) [] l
{-
enleve les élement de x à l
-}
enlever :List a -> List a -> List a 
enlever x l =
    List.foldr(\elt acc -> 
        if List.member elt  x
        then acc
        else elt::acc) [] l




groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy eq xsp =
  case xsp of
    [] -> []
    (x::xs) -> let (ys,zs) = span (eq x) xs
               in (x::ys)::groupBy eq zs

span : (a -> Bool) -> List a -> (List a, List a)
span p xs = (takeWhile p xs, dropWhile p xs)

takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []

{-| Drop elements in order as long as the predicate evaluates to `True`
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then dropWhile predicate xs
               else list

factoriseReponsesDonnees   : List ( Int, Int, Bool )

                         ->   List ( Int, Int,Bool )
{-
indique les réponses cochées par la personne répondant au quiz.
-}
factoriseReponsesDonnees l = 
    let
        listeAvance= 
            List.sortWith (\(a,_,_)(x,_,_) -> compare a x) l
            |> groupBy (\(id1,_,_) (id2,_,_) -> id1 == id2 )
            |> List.map (List.sortWith (\(_,a,_)(_,x,_) -> compare a x))
            |>List.concatMap (groupBy (\(_,idr1,_) (_,idr2,_) -> idr1 == idr2 ))
            |> List.filter (\sousliste -> modBy 2 (List.length sousliste) /= 0)
            |> List.map List.head
            |> List.foldr(\elt acc -> case elt of    
                                            Just(x,y,b) -> (x,y,b)::acc
                                            _ -> acc) []


    
    in 
        listeAvance

        