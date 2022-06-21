module Services.Codec where


decode :: String -> String -> String
decode encodedStr keypass = encodedStr


encode :: String -> String -> String
encode decodedStr keypass = decodedStr