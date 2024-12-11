actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >>  putChar '\n'


doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

doecho3 = do
    l1 <- getLine
    l2 <- getLine
    putStrLn $ l1 ++ l2

dodialog = do
    putStr "What is your happy number? "
    l1 <- getLine
    let num = read l1 :: Int in
        if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

twoQuestions :: IO ()
twoQuestions = do
  putStr "What is your name? "
  name <- getLine
  putStr "How old are you? "
  age <- getLine
  print (name,age)


twoQ :: IO ()
twoQ = putStr "What is your name? "
    >>= \_ -> getLine 
    >>= \l1 -> putStr "How old are you? "
    >>= \_ -> getLine 
    >>= \l2 -> print(l1,l2)

