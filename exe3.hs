import Log

parseMessage :: String -> LogEntry
parseMessage s = case xs of
                  ("I":time:rest)           -> LogMessage Info (read time) (unwords rest)
                  ("W":time:rest)           -> LogMessage Warning (read time) (unwords rest)
                  ("E":category:time:rest)  -> LogMessage (Error (read category)) (read time) (unwords rest)
                  _                         -> Unknown (unwords xs)
                  where xs = words s

{-              | xs == ("I":time:rest) = LogMessage Info (read time) (unwords rest)
                | xs == ("W":time:rest) = LogMessage Warning (read time) (unwords rest)
                | xs == ("E":category:time:rest) = LogMessage (Error (read category)) (read time) (unwords rest)
                | otherwise = Unknown (unwords xs)
                where xs = words s
-}

parseLog :: String -> [LogEntry]
parseLog txt = map parseMessage (lines txt)

insert :: LogEntry -> MessageTree -> MessageTree
insert n@(LogMessage _ _ _) Empty = Node n Empty Empty
insert (Unknown _) tree = tree
insert n1@(LogMessage _ time _) (Node n2@(LogMessage _ time2 _) esq dir)
    | time >= time2 = Node n2 esq (insert n1 dir)
    | time < time2  = Node n2 (insert n1 esq) dir
--  | otherwise = Node (LogMessage _ time2 _) esq (insert (LogMessage _ time _) dir)

--insert (LogMessage _ time _) (Node (LogMessage _ time2 _) esq dir)
--          | time >= time2 = Node (LogMessage _ time2 _) esq (insert (LogMessage _ time _) dir)
--          | time < time2  = Node (LogMessage _ time2 _) (insert (LogMessage _ time _) esq) dir

build :: [LogEntry] -> MessageTree
build xs = foldr insert Empty xs

inOrder :: MessageTree -> [LogEntry]
inOrder Empty = []
inOrder (Node x esq dir) = inOrder esq ++ [x] ++ inOrder dir
