import qualified Data.List
import qualified Data.Set
import Distribution.SPDX (License(NONE))
import Data.Data (Data)
import qualified Data.Array
import qualified Data.Data as Data
import qualified Data.Map
import qualified Data.Maybe
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]
--Retorna lista com todas as cidades
--Percorre a lista de Roadmaps, mapeia os pares de cidades, que são posteriormente concatenados
--a função nub remove duplicados -> mantendo apenas a primeira ocurrência
cities :: RoadMap -> [City]
cities [] = []
cities l = Data.List.nub $ concatMap (\(c1, c2, _) -> [c1, c2]) l



--Retorna True se as cidades  introduzidas encontram-se ligadas e false se não
--Verifica se as possíveis combinações 
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent l x1 x2 = any (\(c1,c2,_)->(c1==x1 && c2==x2)||(c1==x2 && c2==x1)) l


--Retorna a distancia entre duas cidades
--Retorna nothing se as cidades nao estiverem diretamente conectadas ou se o Roadmap for vazio
--Procura pelo primeiro tuplo que tem as duas cidades e retorna a sua distancia
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance l x1 x2 = case Data.List.find  (\(c1,c2,_)->(c1==x1 && c2==x2)||(c1==x2 && c2==x1)) l of
                        Just (_, _, d) -> Just d
                        Nothing       -> Nothing


--Retorna uma lista de cidades adjacentes a introduzida e a distancia das mesmas
--concatena duas listas para o caso de aparecer primeiro a cidade introduzida ou outra
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent l x1 = [(c, d) | (c1, c, d) <- l, c1 == x1] ++ [(c, d) | (c, c2, d) <- l, c2 == x1]


--se o path for nulo ou so uma cidade retorna Just 0
-- calcula a distancia percorrida ao realizar um percurso fornecido, caso este seja possível
--torna o percuro ["a", "b", "c"], numa lista com tuplos das ligações [("a","b"), ("b","c")] e soma as distancias entre estas cidades com a funçao distance
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0                     
pathDistance _ [_] = Just 0         

pathDistance roadMap path = do
    distances <- mapM (\(c1, c2) -> distance roadMap c1 c2) (zip path (tail path)) 
    return (sum distances)                                




--retorna o nome das cidades com maior grau (ligadas a mais cidades)
--cityCounts conta quantas vezes cada cidade aparece na primeira ou segunda posiçao
-- maxdegree retorna todas que têm o grau maximo
rome :: RoadMap -> [City]
rome roadMap =
    let
        allCities = cities roadMap
        cityCounts = map (\city -> (city, length [() | (c1, c2, _) <- roadMap, city == c1 || city == c2])) allCities
        maxDegree = maximum (map snd cityCounts) -- snd verifica apenas o segundo elemento do tuplo

    in [city | (city, count) <- cityCounts, count == maxDegree] --filtra as cidades, escolhendo o nome daquelas que têm count = maxdegree





-- funçao auxiliar de isStronglyConnected
-- faz logica do dfs
-- avança cidades visitadas
-- adiciona ao set de visitadas as nao visitadas e com a funçao adjacent faz a mesma verificação para as adjacentes desta 
reachable :: RoadMap -> City -> Data.Set.Set City
reachable roadMap start = dfs Data.Set.empty [start]
  where
    dfs visited [] = visited
    dfs visited (head:tail)
      | head `Data.Set.member` visited = dfs visited tail
      | otherwise = dfs (Data.Set.insert head visited) (map fst (adjacent roadMap head) ++ tail)--adiciona o nome da cidade e concatena com a tail(resto das cidades a verificar)


--verifica todas as cidades no grafo
-- começa na primeira cidade fornecida
--utiliza a função auxiliar reachable para percorrer o grafo com um dfs
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap =
  let
      allCities = cities roadMap
      startCity = if null allCities then "" else head allCities
      reachableStart = reachable roadMap startCity

  in not (null allCities) && Data.Set.fromList allCities == reachableStart --se verificar estas condições retorna true



--encontra o melhor percurso entre 2 cidades -> retorna varios caso haja mais do que um com a menor distancia

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end
  | start == end = [[start]] -- caso as cidades de inicio e fim sejam as mesmas 
  | otherwise = bfs [([start], 0)] Data.Set.empty []  Nothing -- caso contrario faz um bfs -> um set é utilizado para guardas as cidades visitadas
  
  where
    bfs :: [(Path, Distance)] -> Data.Set.Set City -> [Path] -> Maybe Distance -> [Path] --utiliza uma queue
    bfs [] _ shortest_paths _ = shortest_paths  --nao ha mais caminhos

    bfs ((path, path_dist):queue) visited shortest_paths minDist 
      | current_city == end = --chegou ao fim ->verifica como processar o novo caminho
          
          case minDist of
            Nothing -> bfs queue visited [path] (Just path_dist)  -- primeiro caminho encontrado (como nesse instante é unico é o menor)
            
            Just mi
              | path_dist == mi -> bfs queue visited (path : shortest_paths) minDist --o path tem a mesma distancia que o anterior
              | path_dist < mi -> bfs queue visited [path] (Just path_dist) -- novo shortest path -> menor distancia que os anteriores
              | otherwise -> bfs queue visited shortest_paths minDist  -- caminho maior, logo é ignorado

      | otherwise = --caso a cidade nao seja  final gera novos caminhos com todas as cidades adjacentes 
          
          let new_paths = [ (new_city : path, path_dist + dist) --cria novo path
                         | (new_city, dist) <- adjacent roadMap current_city --pega  nos adjacentes da cidade corrente
                         , new_city `Data.Set.notMember` visited --garante que apenas cidades nao visitadas sao adicionadas para eviar ciclos
                         , maybe True (path_dist + dist <=) minDist --verifica se a distancia é menor que minDist para cancelar o path quando este fica maior que mindist
                         ]
              new_visited = Data.Set.insert current_city visited 
          in bfs (queue ++ new_paths) new_visited shortest_paths minDist --chamada recursiva do bfs
      
      where
        current_city = head path --define a current_city como a primeira cidade no percurso a ser explorado



--função auxilar ao travelSales
--cria uma lista de tuplos de cidades adjacentes  ["A","B","C","D"] -> [("A","B"),("B","C"), ("C","D")]
buildAdjList :: RoadMap -> Data.Map.Map City [(City, Distance)]
buildAdjList roadMap = Data.Map.unionWith (++) 
    (Data.Map.fromListWith (++) [(c1, [(c2, d)]) | (c1, c2, d) <- roadMap]) 
    (Data.Map.fromListWith (++) [(c2, [(c1, d)]) | (c1, c2, d) <- roadMap])

-- solução para o Traveling Salesman Problem
--chama as funções auxiliares com a logica do problema
travelSales :: RoadMap -> Path
travelSales roadMap =
    let adjList = buildAdjList roadMap
        citiesList = cities roadMap
        
    in Data.Maybe.fromMaybe [] (tspDynamic adjList citiesList)  
    
--função auxilar ao travelSales
tspDynamic :: Data.Map.Map City [(City, Distance)] -> [City] -> Maybe Path
tspDynamic adjList citiesList = 
    let startCity = head citiesList
        n = length citiesList
        dpTable = Data.Map.empty  -- tabela de memorização que guarda valores anteriores para não voltar a calcular

    in case findTSP adjList startCity startCity (Data.Set.singleton startCity) dpTable of 
           Just (_, path) -> Just (reverse path ++ [startCity])  -- extrai o caminho e adiciona a cidade inicial de modo a retornar
           Nothing -> Nothing 



--função auxilar ao travelSales
findTSP :: Data.Map.Map City [(City, Distance)] -> City -> City -> Data.Set.Set City
        -> Data.Map.Map (City, Data.Set.Set City) (Maybe (Distance, Path)) -> Maybe (Distance, Path)
findTSP adjList start current visited memo
    | Data.Set.size visited == length (Data.Map.keys adjList) =  -- todas as cidades foram visitadas
        case lookupDistance adjList current start of --depois de visitar todas as cidades verifica se existe um percurso direto para a cidade inicial
            Just returnDistance -> Just (returnDistance, [start])  
            Nothing -> Nothing  -- sem ciclo
    | otherwise =
        case Data.Map.lookup (current, visited) memo of --verifica se foi guardado em momoria antes de computar
            Just result -> result
            Nothing ->
                -- Explore all adjacent unvisited cities and find the shortest path
              let unvisitedPaths = Data.Maybe.mapMaybe (\(next, dist) -> --gera caminhos ao explorar cidades vizinhas nao visitadas 
                    if Data.Set.member next visited then Nothing
                    else fmap (\(nextDist, path) -> (dist + nextDist, next : path)) 
                         (findTSP adjList start next (Data.Set.insert next visited) memo) --chamada recursiva para continuar caminho
                    ) (adjacentCities adjList current)
                  bestPath = if null unvisitedPaths --encontra o melhor caminho dentro das paths possiveis
                               then Nothing
                               else Just (minimumBy fst unvisitedPaths)

                  newMemo = Data.Map.insert (current, visited) bestPath memo --adiciona o resultado a tabela de memorização

                in bestPath
                
  where
    adjacentCities adjList city = Data.Maybe.fromMaybe [] (Data.Map.lookup city adjList)
    lookupDistance adjList c1 c2 = lookup c2 =<< Data.Map.lookup c1 adjList


--função auxilar ao travelSales
--encontra o elemento da lista que tem menor valor
minimumBy :: Ord b => (a -> b) -> [a] -> a
minimumBy f xs = foldr1 (\x y -> if f x < f y then x else y) xs



tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",10),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]


gTest11 :: RoadMap
gTest11 = [
    ("0", "1", 2), ("0", "2", 9), ("0", "3", 10), ("0", "4", 7), ("0", "5", 3),
    ("1", "2", 6), ("1", "3", 4), ("1", "4", 3), ("1", "5", 8), ("1", "6", 5),
    ("2", "3", 8), ("2", "4", 5), ("2", "5", 7), ("2", "6", 4), ("2", "7", 6),
    ("3", "4", 1), ("3", "5", 9), ("3", "6", 2), ("3", "7", 3), ("3", "8", 4),
    ("4", "5", 2), ("4", "6", 6), ("4", "7", 5), ("4", "8", 7), ("4", "9", 3),
    ("5", "6", 4), ("5", "7", 8), ("5", "8", 6), ("5", "9", 5),
    ("6", "7", 1), ("6", "8", 3), ("6", "9", 4),
    ("7", "8", 2), ("7", "9", 6),
    ("8", "9", 1)]