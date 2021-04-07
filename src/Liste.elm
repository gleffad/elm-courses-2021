module Liste exposing (..)

import Html


longueurListe : List a -> Int
longueurListe liste =
    if List.isEmpty liste then
        0

    else
        1 + longueurListe (List.drop 1 liste)



--                 longueurListe [1, 2, 3]
-- ==> 1         + longueurListe [2, 3]
-- ==> 1 + 1     + longueurListe [3]
-- ==> 1 + 1 + 1 + longueurListe []
-- ==> 1 + 1 + 1 + 0


lengthList : List a -> Int
lengthList liste =
    case liste of
        [] ->
            0

        hd :: tl ->
            1 + lengthList tl



--                                         lengthList [1, 2, 3]
-- hd == 1 && tl == [2, 3] ==> 1         + lengthList [2, 3]
-- hd == 2 && tl == [3]    ==> 1 + 1     + lengthList [3]
-- hd == 3 && tl == []     ==> 1 + 1 + 1 + lengthList []
-- []                      ==> 1 + 1 + 1 + 0


isInTheList : List Int -> Int -> Bool
isInTheList list nb =
    case list of
        [] ->
            False

        h :: t ->
            if h == nb then
                True

            else
                isInTheList t nb



-- Filtre les pairs
-- Écrire une fonction sélectionnant uniquement les éléments en position impair dans une liste.
-- evenFilterListEtape : List a -> List a -> List a -> List a
-- evenFilterListEtape init list liste2 =
--   case list of
--     [] -> List.reverse liste2
--     h :: t ->
--       if modBy 2 (indexOf h init) /= 0 then
--         evenFilterListEtape init t (h :: liste2)
--       else
--         evenFilterListEtape init t liste2
-- evenFilterList : List a -> List a
-- evenFilterList list =
--   evenFilterListEtape list list []


isOdd : Int -> Bool
isOdd n =
    modBy 2 n /= 0


pairList_ : List a -> List a -> List a
pairList_ l ret =
    case l of
        [] ->
            ret

        h :: t ->
            if isOdd (lengthList l) then
                pairList_ t (ret ++ [ h ])

            else
                pairList_ t ret


filterList_ : (a -> Bool) -> List a -> List a -> List a
filterList_ f toFilter ret =
    case toFilter of
        [] ->
            ret

        h :: t ->
            if f h then
                filterList_ f t (ret ++ [ h ])

            else
                filterList_ f t ret


filterList : (a -> Bool) -> List a -> List a
filterList f l = filterList_ f l []


lessThanThree : Int -> Bool
lessThanThree n = n > 3


main : Html.Html msg
main =
    Html.text ("Result: " ++ Debug.toString (filterList lessThanThree [ 1, 2, 3, 4, 5, 6 ]))
