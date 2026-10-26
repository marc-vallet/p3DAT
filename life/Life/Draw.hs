-- Marc Vallet

module Life.Draw where
import Life.Board
import Drawing

drawBoard :: Board -> Drawing
drawBoard board =
    -- A completar per l'estudiant
    drawLiveCells board <> drawGrid (minLiveCell board) (maxLiveCell board)

-- Converteix totes les cel.lules vives del tauler en un sol dibuix.
drawLiveCells :: Board -> Drawing
drawLiveCells board =
    foldMap drawLiveCell (liveCells board)

-- Dibuixa una cel.lula viva com un quadrat negre d'una unitat de costat.
drawLiveCell :: Pos -> Drawing
drawLiveCell (col, row) =
    translated (fromIntegral col, fromIntegral row) $
        colored black $ solidRectangle 1 1

-- Proves relacionades amb drawLiveCell i drawBoard.
-- Comanda des del directori life: make dbg-repl
-- ghci> drawLiveCell (3, -2) == translated (3, -2) (colored black (solidRectangle 1 1))
-- True
-- ghci> let provaMarc = foldr (setCell True) initBoard [(3, -2), (3, -1), (3, 0)]
-- ghci> drawBoard provaMarc == (foldMap drawLiveCell (liveCells provaMarc) <> drawGrid (minLiveCell provaMarc) (maxLiveCell provaMarc))
-- True
-- Prova visual: make build
-- Resultat esperat: la compilacio acaba sense errors i crea life/public/all.js.
-- Comanda seguent: make serve
-- Resultat esperat: el navegador mostra les cel.lules vives com quadrats negres
-- i una graella grisa que cobreix el rectangle delimitat pel tauler.

-- 'drawGrid minPos maxPos' obte el dibuix d'una graella que inclou els 2 extrems indicats.
-- 'minPos' es la posicio de l'extrem (esquerra, inferior) i
-- 'maxPos' es la posicio de l'extrem (dreta, superior)'.
-- NOTA: Aquesta funcio s'usa a partir del segon pas de la practica.
drawGrid :: Pos -> Pos -> Drawing
drawGrid minPos maxPos =
    colored gray $
        foldMap vline [fst minPos .. fst maxPos + 1]
        <> foldMap hline [snd minPos .. snd maxPos + 1]
    where
        hline row = polyline [ posToCoords (fst minPos, row), posToCoords (fst maxPos + 1, row) ]
        vline col = polyline [ posToCoords (col, snd minPos), posToCoords (col, snd maxPos + 1) ]
        posToCoords (col, row) = (fromIntegral col - 0.5, fromIntegral row - 0.5)

