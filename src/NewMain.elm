module NewMain exposing (..)

import Html
import Set exposing (Set)


palyndrome : String -> Bool
palyndrome str =
    let chars = String.toList str in
    List.reverse chars == chars


laval : String
laval = "laval"

paspalyndrome : String
paspalyndrome = "jenesuispasunpalyndrome"


otherPalyndrome : String -> Bool
otherPalyndrome possiblePal =
  let
    strLen = String.length possiblePal
  in
    if strLen == 0 || strLen == 1 then
      True
    else
      let
        listPal : List Char
        listPal = Debug.log "mylist" (String.toList possiblePal)

        firstElem : Maybe Char
        firstElem = List.head listPal

        lastElem : Maybe Char
        lastElem = List.head (List.reverse listPal)
      in
        if firstElem == lastElem then
          otherPalyndrome (String.slice 1 (strLen - 1) possiblePal)
        else
          False


alphabet : String
alphabet = "abcdefghijklmnopqrstuvwxyz"

-- Acc
-- 0: "abcdefghijklmnopqrstuvwxyz"
-- 1: (l) "abcdefghijkmnopqrstuvwxyz"
-- 2: (a) "bcdefghijkmnopqrstuvwxyz"
-- 3: (v) "bcdefghijkmnopqrstuwxyz"
-- 4: (a) "bcdefghijkmnopqrstuwxyz"
-- 5: (l) "bcdefghijkmnopqrstuwxyz"
-- ...
-- n: (y) ""

pangram : String -> Bool
pangram possiblePangram =
  let
    alpha1 = Set.fromList (String.toList alphabet)
    myReduce char alpha2 = Set.remove char alpha2
  in
    Set.size (String.foldl myReduce alpha1 possiblePangram) == 0


fold : (Char -> Set Char -> Set Char) -> Set Char -> String -> Set Char
fold reduce acc toReduce =
  let chars = String.toList toReduce
  in
    case chars of
      [] -> acc
      val :: rest ->
        let newAcc = reduce val acc in
        fold reduce newAcc (String.fromList rest)


main : Html.Html msg
main =
    Html.text ("Result: " ++ (Debug.toString (pangram "lavaljlaubpoauietsrnxxkqfmnrnstzzldvdvlabcdefghijklymnopqrstuvwy")))
