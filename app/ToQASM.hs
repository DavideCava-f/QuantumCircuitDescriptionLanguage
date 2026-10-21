module ToQASM where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (toLower)

import CircuitGraph (TransformedGate(..), FinalCircuit, Label(..))

type WireMap = Map.Map Label Int

-- 1. Estrazione delle etichette di ingresso
gateInputs :: TransformedGate -> [Label]
gateInputs (SingleGate _ lIn _)     = [lIn]
gateInputs (FullCNOT cIn _ tIn _) = [cIn, tIn]
gateInputs (GateI lIn _)           = [lIn]

-- 2. Estrazione delle etichette di uscita
gateOutputs :: TransformedGate -> [Label]
gateOutputs (SingleGate _ _ lOut)      = [lOut]
gateOutputs (FullCNOT _ cOut _ tOut) = [cOut, tOut]
gateOutputs (GateI _ lOut)           = [lOut]

-- 3. Verifica se la porta ha tutti gli ingressi pronti
isReady :: [Label] -> TransformedGate -> Bool
isReady available g = all (`elem` available) (gateInputs g)

-- 4. Ordinamento causale/topologico
sortGatesCausally :: FinalCircuit -> FinalCircuit
sortGatesCausally gates = loop initialAvailable gates
  where
    allOutputs = concatMap gateOutputs gates
    allInputs  = concatMap gateInputs gates
    initialAvailable = [ l | l <- allInputs, l `notElem` allOutputs ]

    loop _ [] = []
    loop available remaining =
      let ready    = filter (isReady available) remaining
          notReady = filter (not . isReady available) remaining
      in case ready of
           [] -> remaining
           _  -> let newProduced   = concatMap gateOutputs ready
                     nextAvailable = available ++ newProduced
                 in ready ++ loop nextAvailable notReady

-- 5. Generazione del codice OpenQASM
toQasm :: FinalCircuit -> String
toQasm unsortedgates =
  let gates = sortGatesCausally unsortedgates
      (qasmLines, _, numQubits) = foldl processGate ([], Map.empty, 0) gates
      header = [ "OPENQASM 3;"
               , "include \"stdgates.inc\";"
               , "qubit[" ++ show numQubits ++ "] q;"
               ]
  in unlines (header ++ reverse qasmLines)
  where
    processGate :: ([String], WireMap, Int) -> TransformedGate -> ([String], WireMap, Int)
    
    -- Gestione della porta Identità esplicita (GateI)
    processGate (acc, wmap, nextQubit) (GateI lIn lOut) =
      let (qIndex, wmap', nextQ') = getOrAllocate lIn wmap nextQubit
          wmap'' = Map.insert lOut qIndex (Map.delete lIn wmap')
          line   = "id q[" ++ show qIndex ++ "];"
      in (line : acc, wmap'', nextQ')

    -- Gestione di SingleGate generico (es. H, X, Y, Z, T o "I")
    processGate (acc, wmap, nextQubit) (SingleGate gName lIn lOut) =
      let (qIndex, wmap', nextQ') = getOrAllocate lIn wmap nextQubit
          wmap'' = Map.insert lOut qIndex (Map.delete lIn wmap')
          opName = if gName == "I" then "id" else map toLower gName
          line   = opName ++ " q[" ++ show qIndex ++ "];"
      in (line : acc, wmap'', nextQ')

    -- Gestione della CNOT
    processGate (acc, wmap, nextQubit) (FullCNOT cIn cOut tIn tOut) =
      let (cIndex, wmap1, nextQ1) = getOrAllocate cIn wmap nextQubit
          (tIndex, wmap2, nextQ2) = getOrAllocate tIn wmap1 nextQ1
          wmap3 = Map.insert cOut cIndex . Map.insert tOut tIndex $ 
                  Map.delete cIn (Map.delete tIn wmap2)
          line  = "cnot q[" ++ show cIndex ++ "], q[" ++ show tIndex ++ "];"
      in (line : acc, wmap3, nextQ2)

    -- Inserisce una nuova label o recupera l'indice del qubit se già presente
    getOrAllocate :: Label -> WireMap -> Int -> (Int, WireMap, Int)
    getOrAllocate l wmap nextQ =
      case Map.lookup l wmap of
        Just qIdx -> (qIdx, wmap, nextQ)
        Nothing   -> (nextQ, Map.insert l nextQ wmap, nextQ + 1)
