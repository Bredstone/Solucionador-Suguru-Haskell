import Data.List ( intercalate )
import Data.Maybe ( isNothing, fromJust )

-- Estruturas:
-- ---------------------------------------------------------------------------------------------------------------------
-- Célula
data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
-- Posição
type Pos = (Int, Int)
-- Região
type Region = [Pos]
-- Suguru
type Suguru = ([[Cell]], [Region])

-- Exemplos de sugurus:
-- ---------------------------------------------------------------------------------------------------------------------
-- Exemplo de suguru simples 5 x 5, com 6 regiões
suguru_1 :: Suguru
suguru_1 = (
  -- Células
  [
    -- Linha 0
    [Fixed 1, Possible [1..4], Possible [1..4], Fixed 5, Possible [1..5]],
    -- Linha 1
    [Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 2
    [Fixed 1, Possible [1..5], Fixed 2, Possible [1..5], Fixed 4],
    -- Linha 3
    [Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 4
    [Possible [1..5], Fixed 3, Possible [1..5], Possible [1..5], Possible [1]]
  ],

  -- Regiões
  [
    -- Região 0
    [(0,0), (0,1), (0,2), (1,0)],
    -- Região 1
    [(0,3), (0,4), (1,4), (2,4), (3,4)],
    -- Região 2
    [(1,1), (1,2), (2,0), (2,1), (3,0)],
    -- Região 3
    [(1,3), (2,2), (2,3), (3,1), (3,2)],
    -- Região 4
    [(3,3), (4,0), (4,1), (4,2), (4,3)],
    -- Região 5
    [(4,4)]
  ])

-- Exemplo de suguru 6 x 6, com 8 regiões (Suguru Nº 1)
suguru_2 :: Suguru
suguru_2 = (
  -- Células
  [
    -- Linha 0
    [Fixed 4, Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 1
    [Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 2
    [Possible [1..4], Possible [1..5], Fixed 4, Possible [1..5], Possible [1..5], Fixed 1],
    -- Linha 3
    [Possible [1..5], Possible [1..5], Possible [1..5], Fixed 2, Possible [1..5], Possible [1..5]],
    -- Linha 4
    [Fixed 5, Possible [1..2], Possible [1..2], Fixed 3, Fixed 5, Possible [1..5]],
    -- Linha 5
    [Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]]
  ],

  -- Regiões
  [
    -- Região 0
    [(0, 0), (0, 1), (1, 0), (2, 0)],
    -- Região 1
    [(0, 2), (1, 1), (1, 2), (1, 3), (2, 2)],
    -- Região 2
    [(0, 3), (0, 4), (0, 5), (1, 4), (2, 4)],
    -- Região 3
    [(1, 5), (2, 5), (3, 5), (4, 5), (5, 5)],
    -- Região 4
    [(2, 1), (3, 0), (3, 1), (4, 0), (5, 0)],
    -- Região 5
    [(2, 3), (3, 2), (3, 3), (3, 4), (4, 3)],
    -- Região 6
    [(4, 1), (4, 2)],
    -- Região 7
    [(4, 4), (5, 1), (5, 2), (5, 3), (5, 4)]
  ])

-- Exemplo de suguru 8 x 8, com 15 regiões (Suguru Nº 12)
suguru_3 :: Suguru
suguru_3 = (
  -- Células
  [
    -- Linha 0
    [Fixed 2, Possible [1], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 1
    [Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Fixed 5, Possible[1..5], Fixed 1],
    -- Linha 2
    [Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 3
    [Possible [1..5], Fixed 3, Fixed 5, Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 4
    [Possible [1..4], Possible [1..4], Possible [1..3], Possible [1..3], Possible [1..5], Fixed 4, Possible [1..5], Possible [1..5]],
    -- Linha 5
    [Possible [1..4], Fixed 5, Possible [1..3], Fixed 5, Possible [1..5], Possible [1..5], Possible [1..5], Fixed 3],
    -- Linha 6
    [Possible [1..5], Fixed 4, Possible [1..5], Fixed 3, Possible [1..5], Possible [1..5], Fixed 2, Fixed 5],
    -- Linha 7
    [Possible [1], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]]
  ],

  -- Regiões
  [
    -- Região 0
    [(0,0), (1,0), (2,0), (2,1), (3,0)],
    -- Região 1
    [(0,1)],
    -- Região 2
    [(0,2), (1,1), (1,2), (1,3), (2,2)],
    -- Região 3
    [(0,3), (0,4), (0,5), (0,6), (1,4)],
    -- Região 4
    [(0,7), (1,6), (1,7), (2,7), (3,7)],
    -- Região 5
    [(1,5), (2,4), (2,5), (2,6), (3,5)],
    -- Região 6
    [(2,3),(3,2),(3,3),(3,4),(4,4)],
    -- Região 7
    [(3,1), (4,0), (4,1), (5,0)],
    -- Região 8
    [(3,6), (4,6), (4,7), (5,7), (6,7)],
    -- Região 9
    [(4,2), (4,3), (5,2)],
    -- Região 10
    [(4,5), (5,4), (5,5), (5,6), (6,5)],
    -- Região 11
    [(5,1), (6,0), (6,1), (6,2), (7,1)],
    -- Região 12
    [(5,3), (6,3), (6,4), (7,2), (7,3)],
    -- Região 13
    [(6,6), (7,4), (7,5), (7,6), (7,7)],
    -- Região 14
    [(7,0)]
  ])

-- Exemplo de suguru 10 x 10, com 18 regiões (Suguru Nº 160)
suguru_4 :: Suguru
suguru_4 = (
  -- Células
  [
    -- Linha 0
    [Fixed 6, Fixed 2, Fixed 5, Fixed 3, Possible [1..7], Fixed 7, Possible [1..7], Fixed 7, Fixed 2, Possible [1..5]],
    -- Linha 1
    [Fixed 5, Fixed 4, Possible [1..7], Fixed 2, Fixed 1, Possible [1..7], Fixed 3, Possible [1..7], Possible [1..5], Fixed 1],
    -- Linha 2
    [Fixed 3, Possible [1..6], Fixed 3, Possible [1..7], Fixed 5, Possible [1..7], Fixed 2, Possible [1..7], Possible [1..5], Fixed 7],
    -- Linha 3
    [Possible [1..6], Fixed 4, Fixed 2, Fixed 6, Possible [1..7], Possible [1..7], Possible [1..7], Fixed 4, Possible [1..5], Fixed 4],
    -- Linha 4
    [Fixed 1, Fixed 5, Possible [1..7], Possible [1..7], Fixed 1, Fixed 4, Possible [1..6], Fixed 2, Fixed 6, Fixed 2],
    -- Linha 5
    [Fixed 7, Possible [1..7], Fixed 6, Possible [1..7], Fixed 3, Possible [1..6], Fixed 3, Possible [1], Fixed 3, Possible [1..7]],
    -- Linha 6
    [Possible [1], Fixed 4, Possible [1..6], Possible [1..6], Possible [1..6], Fixed 5, Fixed 2, Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 7
    [Possible [1..6], Possible [1..6], Possible [1..7], Fixed 7, Fixed 2, Possible [1..7], Possible [1..7], Fixed 5, Possible [1..5], Fixed 5],
    -- Linha 8
    [Possible [1..7], Possible [1..7], Fixed 4, Possible [1..7], Fixed 4, Fixed 7, Possible [1..7], Possible [1..7], Fixed 6, Possible [1..5]],
    -- Linha 9
    [Fixed 5, Possible [1..2], Possible [1..2], Possible [1..7], Fixed 6, Possible [1..7], Fixed 5, Possible [1..5], Fixed 3, Fixed 2]
  ],

  -- Regiões
  [
    -- Região 0
    [(0,0), (1,0), (2,0), (2,1), (3,0), (3,1)],
    -- Região 1
    [(0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (1,1)],
    -- Região 2
    [(0,7), (1,2), (1,3), (1,4), (1,5), (1,6), (1,7)],
    -- Região 3
    [(0,8), (0,9), (1,8), (2,8), (3,8)],
    -- Região 4
    [(1,9), (2,9), (3,9), (4,8), (4,9), (5,8), (5,9)],
    -- Região 5
    [(2,2), (2,3), (3,2), (3,3), (3,4), (4,2), (4,3)],
    -- Região 6
    [(2,4), (2,5), (2,6), (2,7), (3,5), (3,6), (3,7)],
    -- Região 7
    [(4,0), (4,1), (5,0), (5,1), (5,2), (5,3), (5,4)],
    -- Região 8
    [(4,4), (4,5), (4,6), (4,7), (5,5), (5,6)],
    -- Região 9
    [(5,7)],
    -- Região 10
    [(6,0)],
    -- Região 11
    [(6,1), (6,2), (6,3), (6,4), (7,0), (7,1)],
    -- Região 12
    [(6,5), (6,6), (7,5), (7,6), (8,6), (8,7), (8,8)],
    -- Região 13
    [(6,7), (6,8), (6,9), (7,7), (7,8)],
    -- Região 14
    [(7,2), (7,3), (7,4), (8,0), (8,1), (8,2), (9,0)],
    -- Região 15
    [(7,9), (8,9), (9,7), (9,8), (9,9)],
    -- Região 16
    [(8,3), (8,4), (8,5), (9,3), (9,4), (9,5), (9,6)],
    -- Região 17
    [(9,1), (9,2)]
  ])

-- Exemplo de suguru vazio 6 x 6, com 8 regiões e uso de backtracking (Suguru Nº 1)
suguru_5 :: Suguru
suguru_5 = (
  -- Células
  [
    -- Linha 0
    [Possible [1..4], Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 1
    [Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 2
    [Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 3
    [Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 4
    [Possible [1..5], Possible [1..2], Possible [1..2], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 5
    [Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]]
  ],

  -- Regiões
  [
    -- Região 0
    [(0, 0), (0, 1), (1, 0), (2, 0)],
    -- Região 1
    [(0, 2), (1, 1), (1, 2), (1, 3), (2, 2)],
    -- Região 2
    [(0, 3), (0, 4), (0, 5), (1, 4), (2, 4)],
    -- Região 3
    [(1, 5), (2, 5), (3, 5), (4, 5), (5, 5)],
    -- Região 4
    [(2, 1), (3, 0), (3, 1), (4, 0), (5, 0)],
    -- Região 5
    [(2, 3), (3, 2), (3, 3), (3, 4), (4, 3)],
    -- Região 6
    [(4, 1), (4, 2)],
    -- Região 7
    [(4, 4), (5, 1), (5, 2), (5, 3), (5, 4)]
  ])

-- Exemplo de suguru vazio 7 x 6, com 8 regiões e uso de backtracking
suguru_6 :: Suguru
suguru_6 = (
  -- Células
  [
    -- Linha 0
    [Possible [1..4], Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 1
    [Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..6]],
    -- Linha 2
    [Possible [1..4], Possible [1..6], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..6]],
    -- Linha 3
    [Possible [1..6], Possible [1..6], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..6]],
    -- Linha 4
    [Possible [1..6], Possible [1..2], Possible [1..2], Possible [1..5], Possible [1..9], Possible [1..6]],
    -- Linha 5
    [Possible [1..6], Possible [1..9], Possible [1..9], Possible [1..9], Possible [1..9], Possible [1..6]],
    -- Linha 6
    [Possible [1..6], Possible [1..9], Possible [1..9], Possible [1..9], Possible [1..9], Possible [1..6]]
  ],

  -- Regiões
  [
    -- Região 0
    [(0, 0), (0, 1), (1, 0), (2, 0)],
    -- Região 1
    [(0, 2), (1, 1), (1, 2), (1, 3), (2, 2)],
    -- Região 2
    [(0, 3), (0, 4), (0, 5), (1, 4), (2, 4)],
    -- Região 3
    [(1, 5), (2, 5), (3, 5), (4, 5), (5, 5), (6, 5)],
    -- Região 4
    [(2, 1), (3, 0), (3, 1), (4, 0), (5, 0), (6, 0)],
    -- Região 5
    [(2, 3), (3, 2), (3, 3), (3, 4), (4, 3)],
    -- Região 6
    [(4, 1), (4, 2)],
    -- Região 7
    [(4, 4), (5, 1), (5, 2), (5, 3), (5, 4), (6, 1), (6, 2), (6, 3), (6, 4)]
  ])

-- Exemplo de suguru inválido 6 x 6, com 8 regiões (Suguru Nº 1)
suguru_7 :: Suguru
suguru_7 = (
  -- Células
  [
    -- Linha 0
    [Possible [1..4], Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 1
    [Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 2
    [Possible [1..4], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 3
    [Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 4
    [Possible [1..5], Fixed 1, Fixed 1, Possible [1..5], Possible [1..5], Possible [1..5]],
    -- Linha 5
    [Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5], Possible [1..5]]
  ],

  -- Regiões
  [
    -- Região 0
    [(0, 0), (0, 1), (1, 0), (2, 0)],
    -- Região 1
    [(0, 2), (1, 1), (1, 2), (1, 3), (2, 2)],
    -- Região 2
    [(0, 3), (0, 4), (0, 5), (1, 4), (2, 4)],
    -- Região 3
    [(1, 5), (2, 5), (3, 5), (4, 5), (5, 5)],
    -- Região 4
    [(2, 1), (3, 0), (3, 1), (4, 0), (5, 0)],
    -- Região 5
    [(2, 3), (3, 2), (3, 3), (3, 4), (4, 3)],
    -- Região 6
    [(4, 1), (4, 2)],
    -- Região 7
    [(4, 4), (5, 1), (5, 2), (5, 3), (5, 4)]
  ])

-- Helpers suguru:
-- ---------------------------------------------------------------------------------------------------------------------
-- Separa uma lista em uma matriz
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = take n l : group n (drop n l)

-- Retorna uma string para apresentação do suguru
showSuguru :: Maybe Suguru -> String
showSuguru Nothing = "Suguru sem resposta"
showSuguru (Just (suguru, _)) = unlines [unwords (map toString x) | x <- suguru] where
  toString (Fixed v) = show v
  toString (Possible v) = show v

-- Verificações:
-- -----------------------------------------------------------------------------
-- Verifica se o suguru foi resolvido
isSolved :: Suguru -> Bool
isSolved (suguru, _) = all (==True) [isFixed cell | linha <- suguru, cell <- linha]

-- Verifica se um suguru possui solução
hasSolution :: Suguru -> Bool 
hasSolution (suguru, _) = not (or [null (getCellValue cell) | linha <- suguru, cell <- linha])

-- Verifica se a célula é fixa
isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed (Possible _) = False

-- Getters:
-- -----------------------------------------------------------------------------
-- Retorna um conjunto de células, a partir de um conjunto de posições
getCell :: [Pos] -> [[Cell]] -> [Cell]
getCell pos suguru = [suguru!!x!!y | (x, y) <- pos]

-- Retorna um array com os valores de uma célula (possíveis ou fixos)
getCellValue :: Cell -> [Int]
getCellValue (Possible value) = value
getCellValue (Fixed value) = [value]

-- Retorna os valores fixos de um conjunto de células
getFixed :: [Cell] -> [Int]
getFixed cells = concatMap getCellValue [cell | cell <- cells, isFixed cell]

-- Retorna as células adjacentes de uma posição
getAdj :: Pos -> Int -> Int -> [[Cell]] -> [Cell]
getAdj (x, y) nX nY = getCell [(x1, y1) | 
                  (x1, y1) <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y+1), (x-1, y-1), (x-1, y+1)],
                  x1 >= 0, y1 >= 0, x1 <= nX, y1 <= nY]

-- Retorna todas as células de uma região, exceto a célula da posição inicial informada
getRegion :: Pos -> [[Pos]] -> [[Cell]] -> [Cell]
getRegion pos regions = getCell [position | position <- head [linha | linha <- regions, pos `elem` linha], position /= pos]

-- Setters:
-- -----------------------------------------------------------------------------
-- Fixa uma célula, caso ela possua apenas uma possibilidade
fixCell :: Cell -> Cell
fixCell (Fixed v) = Fixed v
fixCell (Possible v) | length v == 1 = Fixed (head v)
                     | otherwise = Possible v

-- Fixa um valor se ele é único dentre as possibilidades
fixUnique :: Cell -> [Int] -> Cell
fixUnique (Fixed fixed) _ = Fixed fixed
fixUnique (Possible poss) values = do
  let uniques = [p | p <- poss, p `notElem` values]
  if length uniques == 1 then Fixed (head uniques) else Possible poss

-- Remove, das possibilidades de uma célula, um conjunto de números especificado
trimCell :: Cell -> [Int] -> Cell
trimCell (Possible poss) values = Possible [p | p <- poss, p `notElem` values]
trimCell (Fixed fixed) values | fixed `elem` values = Possible []
                              | otherwise = Fixed fixed

-- Suguru simple solver:
-- ---------------------------------------------------------------------------------------------------------------------
-- Remove das possibilidades de uma célula ainda não fixada, os números fixados, de sua adjacência
trimAdj :: Suguru -> Suguru
trimAdj (suguru, regions) = do
  let nX = length suguru - 1
  let nY = length (head suguru) - 1
  let indexes = [(x, y) | x <- [0..nX], y <- [0..nY]]
  let newSuguru = group (nY+1) [trimCell (suguru!!x!!y) (getFixed (getAdj (x, y) nX nY suguru)) | (x, y) <- indexes]

  (newSuguru, regions)

-- Remove das possibilidades de uma célula ainda não fixada, os números fixados, de sua região
trimRegion :: Suguru -> Suguru
trimRegion (suguru, regions) = do
  let nX = length suguru - 1
  let nY = length (head suguru) - 1
  let indexes = [(x, y) | x <- [0..nX], y <- [0..nY]]
  let newSuguru = group (nY+1) [trimCell (suguru!!x!!y) (getFixed (getRegion (x, y) regions suguru)) | (x, y) <- indexes]

  (newSuguru, regions)

-- Fixa um número, caso, dentro de uma região, só exista uma única célula onde possa ser inserido
checkUnique :: Suguru -> Suguru
checkUnique (suguru, regions) = do
  let nX = length suguru - 1
  let nY = length (head suguru) - 1
  let indexes = [(x, y) | x <- [0..nX], y <- [0..nY]]
  let newSuguru = group (nY+1) [fixUnique (suguru!!x!!y) (concatMap getCellValue (getRegion (x, y) regions suguru)) | (x, y) <- indexes]

  (newSuguru, regions)

-- Caso uma célula, ainda não fixada, só tenha uma possibilidade, fixa seu valor
fixIt :: Suguru -> Suguru
fixIt (suguru, regions) = do
  let nX = length suguru - 1
  let nY = length (head suguru) - 1
  let indexes = [(x, y) | x <- [0..nX], y <- [0..nY]]
  let newSuguru = group (nY+1) [fixCell (suguru!!x!!y) | (x, y) <- indexes]

  (newSuguru, regions)

-- Tenta resolver o suguru sem backtracking - retorna True se conseguir ou False caso não seja possível
solveSimple :: Suguru -> (Bool, Maybe Suguru)
solveSimple suguru | isSolved suguru = (True, return suguru)
                   | not (hasSolution suguru) = (False, Nothing)
                   | otherwise = do
                      let newSuguru = fixIt (checkUnique (trimRegion (trimAdj suguru)))
                      if newSuguru == suguru then (False, return newSuguru) else solveSimple newSuguru

-- Backtracking:
-- ---------------------------------------------------------------------------------------------------------------------
-- Retorna a primeira célula não fixada de um suguru e sua posição
getPossible :: Maybe Suguru -> ([Int], Pos)
getPossible Nothing = ([], (-1,-1))
getPossible (Just (suguru, regions)) = do
  let nX = length suguru - 1
  let nY = length (head suguru) - 1
  let (x, y) = head [(x, y) | x <- [0..nX], y <- [0..nY], not (isFixed (suguru!!x!!y))]
  (getCellValue (suguru!!x!!y), (x, y))

-- Cria um novo suguru com uma célula inserida em um lugar especificado
tryWith :: Pos -> Suguru -> Cell -> Suguru
tryWith pos (suguru, regions) value = do
  let nX = length suguru - 1
  let nY = length (head suguru) - 1
  let indexes = [(x, y) | x <- [0..nX], y <- [0..nY]]
  (group (nY+1) [if (x, y) == pos then value else suguru!!x!!y | (x, y) <- indexes], regions)

-- Tenta resolver o suguru por meio de backtracking
backtrackSolve :: Maybe Suguru -> ([Int], Pos) -> Maybe Suguru
backtrackSolve Nothing _ = Nothing
backtrackSolve _ ([], _) = Nothing
backtrackSolve (Just suguru) (x:xs, pos) = do
  let solved = solve (tryWith pos suguru (Fixed x))
  if isNothing solved then backtrackSolve (return suguru) (xs, pos)
  else solved

-- Solução final:
-- ---------------------------------------------------------------------------------------------------------------------
-- Resolve o suguru
solve :: Suguru -> Maybe Suguru
solve suguru = do
  let (signal, solved) = solveSimple suguru
  if signal then solved else backtrackSolve solved (getPossible solved)

main = do
  putStrLn (showSuguru (solve suguru_1))
  putStrLn (showSuguru (solve suguru_2))
  putStrLn (showSuguru (solve suguru_3))
  putStrLn (showSuguru (solve suguru_4))
  putStrLn (showSuguru (solve suguru_5))
  putStrLn (showSuguru (solve suguru_6))
  putStrLn (showSuguru (solve suguru_7))