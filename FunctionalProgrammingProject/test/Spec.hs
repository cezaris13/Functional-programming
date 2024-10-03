import Lib2 
import Data.Either ( Either (Left) )
import Lib3
import Lib1
import qualified Data.Either as E
import Data.Function

-- Test data
jsonWithoutStart = "age: 5}"
jsonWithoutEnd = "{age: 5"
jsonWithoutValue = "age"
jsonMissingBracket = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"
jsonInvalidEnding ="{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"tail\":}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"
validJson = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

renderList1 = [RenderData {gameBlock = 'b', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6}]
renderList2 = [RenderData {gameBlock = 'b', y = 1, x = 2},RenderData {gameBlock = 'x', y = 1, x = 7}]

correctCommandJson1 = "{\"command\":{\"direction\":\"Left\",\"name\":\"MoveBomberman\"},\"additional\":{\"command\":{\"name\":\"FetchSurrounding\"},\"additional\":null}}"
correctCommandJson2 = "{\"command\":{\"name\":\"FetchBombSurrounding\"},\"additional\":{\"command\":{\"name\":\"FetchSurrounding\"},\"additional\":null}}"

objectArray :: [(String, [(Int, Int)])]
objectArray = [("bombermans",[(7,9)]),("bricks",[(1,6),(1,7),(1,8),(1,9),(1,10),(1,11),(1,12),(1,13),(2,3),(3,4),(3,6),(4,11),(4,13),(5,4),(5,8),(6,5),(6,7),(6,11),(6,13),(7,10),(7,12),(8,3),(8,7),(8,9),(8,11),(8,13),(10,3),(10,5),(10,7),(10,9),(10,11),(12,3),(12,5),(12,7),(12,9),(12,11),(12,13)]),("gates",[]),("ghosts",[]),("wall",[(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,10),(0,11),(0,12),(0,13),(0,14),(0,15),(0,16),(1,14),(2,2),(2,4),(2,6),(2,8),(2,10),(2,12),(2,14),(3,14),(4,2),(4,4),(4,6),(4,8),(4,10),(4,12),(4,14),(5,14),(6,2),(6,4),(6,6),(6,8),(6,10),(6,12),(6,14),(7,14),(8,2),(8,4),(8,6),(8,8),(8,10),(8,12),(8,14),(9,14),(10,2),(10,4),(10,6),(10,8),(10,10),(10,12),(10,14),(11,14),(12,2),(12,4),(12,6),(12,8),(12,10),(12,12),(12,14),(14,2),(14,3),(14,4),(14,5),(14,6),(14,7),(14,8),(14,9),(14,10),(14,11),(14,12),(14,13),(14,14),(14,15),(14,16)]),("bomb_surrounding",[]),("bomb",[(7,9)])]

-- Test functions
isLeft (Data.Either.Left _) = True
isLeft _        = False

baseErrorMessage = "Error: Did not throw while parsing:  "

testThrow :: ([Char] -> Either a b) -> [Char] -> IO ()
testThrow function parameter = putStrLn $ if isLeft $ function parameter then "OK" else baseErrorMessage ++ "\"" ++ parameter ++ "\""

assertEqualString :: String -> String -> IO ()
assertEqualString x y = putStrLn $ if x == y then "OK" else "Error: " ++ x

assertEqualInt :: Int -> Int -> IO ()
assertEqualInt x y = putStrLn $ if x == y then "OK" else "Error: " ++ show x
   
main :: IO ()
main = do 
    testThrow parseJsonMessage jsonWithoutStart
    testThrow parseJsonMessage jsonWithoutEnd
    testThrow parseJsonMessage jsonWithoutValue
    testThrow parseJsonMessage jsonMissingBracket
    testThrow parseJsonMessage jsonInvalidEnding
    putStrLn $ if not $ isLeft $ parseJsonMessage validJson then "OK" else "Fails to parse valid json"
    lib3test1
    lib3test2
    lib3test3
    lib3test4
    lib3test5
    lib3test6
    return()

lib3test1 :: IO ()
lib3test1 = do
    let extraCommand = Just (Commands FetchSurrounding Nothing)
    let commands = Commands FetchBombSurrounding extraCommand :: Commands
    let x = handle (toJsonLike commands)
    let testOutput = handle (fromJsonLike x) :: String
    assertEqualString testOutput "{\"command\":{\"name\":\"FetchBombSurrounding\"},\"additional\":{\"command\":{\"name\":\"FetchSurrounding\"},\"additional\":null}}"

lib3test2 :: IO ()
lib3test2 = do
    let testString = JsonLikeString "{\"height\":15,\"uuid\":\"baee123b-4608-451a-b777-9bc726e76760\",\"width\":15}"
    let game = handle (fromJsonLike testString) :: NewGame
    assertEqualInt (height game) 15
    assertEqualInt (width game) 15
    assertEqualString (gameId game) "baee123b-4608-451a-b777-9bc726e76760"

lib3test3 = do
    let newBlocks = Lib3.appendBlocks renderList1 renderList2
    if findBomb newBlocks == (1, 1)
        then putStrLn "OK"
        else putStrLn "Appends extra bomb"

lib3test4 = do
    let extraCommand = Just (Commands FetchSurrounding Nothing)
    let command1 = Commands (FetchBombSurrounding) extraCommand :: Commands
    let str = toJsonLike command1 & e & fromJsonLike & e :: String
    assertEqualString str correctCommandJson2

lib3test5 = do
    let extraCommand = Just (Commands FetchSurrounding Nothing)
    let command1 = Commands (MoveBomberman Lib3.Left) extraCommand :: Commands
    let str = toJsonLike command1 & e & fromJsonLike & e :: String
    assertEqualString str correctCommandJson1
 

lib3test6 = do
    let position = findBombInObjects objectArray
    if ((fst (head position)), (snd (head position))) == (7, 9)
        then putStrLn "OK"
        else putStrLn "Could not find the position"
        
e :: Either String a -> a
e = E.either error id
