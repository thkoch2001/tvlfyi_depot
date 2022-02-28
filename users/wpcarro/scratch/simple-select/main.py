from scanner import Scanner

def tokenize(x):
  s = Scanner(x)
  return None

def main():
  while True:
    x = input("> ")
    print(tokenize(x))

if __name__ == "__main__":
  main()
