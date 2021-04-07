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
    [] -> 0
    hd :: tl -> 1 + lengthList tl


--                                         lengthList [1, 2, 3]
-- hd == 1 && tl == [2, 3] ==> 1         + lengthList [2, 3]
-- hd == 2 && tl == [3]    ==> 1 + 1     + lengthList [3]
-- hd == 3 && tl == []     ==> 1 + 1 + 1 + lengthList []
-- []                      ==> 1 + 1 + 1 + 0



main : Html.Html msg
main =
    Html.text ("Result: " ++ (Debug.toString (lengthList [1, 2, 3])))
