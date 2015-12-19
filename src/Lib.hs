module Lib
    ( llamar
    ) where


import Network.Wreq
import Control.Lens
import Data.List(partition)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import System.Directory

data OLX = OLX {
  subdomain :: String
}

data Resultado = EsViejo {
  titulo :: String,
  iid :: Int
  } | EsNuevo {
  link :: String,
  info :: String,
  precio :: String,
  negociable :: Bool, 
  fecha :: String
} | EsError { res :: B.ByteString }

esViejo (EsViejo _ _) = True
esViejo _ = True --CHANGEME
--esViejo EsError _ _ = False

esNuevo = not . esViejo

comunidades :: [String]
comunidades = ["ciudaddeguatemala", "villanueva"]

categorias :: [String]
categorias = ["piso-casa-en-alquiler-cat-363"]

llamadas :: [(String,String)]
llamadas = [(com,cat) | com <- comunidades, cat <- categorias]

desdeListado :: LB.ByteString -> IO [Resultado]
desdeListado html = do
  --putStrLn $ "Se obtuvo " ++ (LazyUTF8.toString html)  
  
  return []
  
encontrarPagina :: (String, String) -> Int -> IO [Resultado]
encontrarPagina llamada@(com, cat) p =
  let
    url' = "http://" ++ com ++ ".olx.com.gt/" ++ cat ++ "-p-" ++ (show p)

    guardarPagina :: Int -> (String,String) -> LB.ByteString -> IO()
    guardarPagina p llamada@(comunidad,categoria) payload = 
      let
        filename = show p ++ ".html" 
        filedir =  "salida/" ++ comunidad ++ "/" ++ categoria ++ "/pagina/"
        path = filedir ++ filename
      in do
        createDirectoryIfMissing True filedir
        writeFile path (LazyUTF8.toString payload)
      
  in do
    r <- get url'
    let payload = r ^. responseBody
    guardarPagina p llamada payload
    desdeListado $ payload
    

buscar :: (String, String) -> IO [Resultado]
buscar llamada@(com,cat) = llamada `encontrarDesde` 1
  where
    encontrarDesde :: (String, String) -> Int -> IO [Resultado]
    encontrarDesde llamada p = do      
      rs <- llamada `encontrarPagina` p
      let (viejos, nuevos) = partition esViejo rs          
          hayViejos = length viejos > 0
          hayNuevos = length nuevos > 0
          
      if (not hayNuevos)
      then return []
      else if (hayViejos) then return nuevos
      else llamada `encontrarDesde` (p+1) >>= \ns -> return $ ns ++ nuevos
  
llamar :: IO ()
llamar = do
  rss <- mapM buscar llamadas >>= return . concat
    
  putStrLn "Hecho."
