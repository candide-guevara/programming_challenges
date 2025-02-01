pStr = ["def separator():",
"  return ',' + chr(0xa)",
"",
"def quote():",
"  return chr(0x22)",
"",
"def printProg(pStr):",
"  refl = 'pStr = [' + separator().join([quote() + i + quote() for i in pStr]) + ']'",
"  print(refl)",
"  for i in range(len(pStr)):",
"    print(pStr[i])",
"",
"printProg(pStr)"]
def separator():
  return ',' + chr(0xa)

def quote():
  return chr(0x22)

def printProg(pStr):
  refl = 'pStr = [' + separator().join([quote() + i + quote() for i in pStr]) + ']'
  print(refl)
  for i in range(len(pStr)):
    print(pStr[i])

printProg(pStr)
