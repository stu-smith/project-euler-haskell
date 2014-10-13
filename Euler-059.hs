--
-- Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
--
-- A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
--
-- For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes. The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.
--
-- Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.
--
-- Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.
--

import Data.Bits
import Data.Char
import Data.List
import Data.Ord

main = do contents <- readFile "cipher1.txt"
          let text = map chr (read ("[" ++ contents ++ "]") :: [Int])
          let decrypted = head $ map (\ k -> decrypt text k) $ validKeys text
          putStrLn $ show $ sum $ map ord decrypted


validKeys s = filter (\ k -> isEnglish $ decrypt s k) allKeys

isEnglish s = allAscii s && all inTop " etaonst"
    where ls      = map toLower s
          inTop x = x `elem` (take 10 $ map head freqs)
          freqs   = reverse $ sortBy (comparing length) $ group $ sort ls

decrypt s k = zipWith combine s key
    where key         = concat $ repeat k
          combine x y = chr ((ord x) `xor` (ord y))

allKeys = do x <- az
             y <- az
             z <- az
             return [x,y,z]
          where az = ['a'..'z']

allAscii = all (\x -> x >= chr 32 && x < chr 123)

