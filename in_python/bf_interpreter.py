import sys

def run(file):
    f = open(file).read()
    interpret(f)

def interpret(s):
    stack = match_braces(s)
    cell_arr, codepos, pointer = [0], 0, 0
    try:
        while codepos < len(s):
            c = s[codepos]
            match c:
                case '>':
                    pointer += 1
                    if pointer == len(cell_arr):
                        cell_arr.append(0)
                case '<':
                    pointer = max(0, pointer - 1)
                case '+':
                    cell_arr[pointer] += 1
                case '-':
                    cell_arr[pointer] -= 1
                case '.':
                    print(chr(cell_arr[pointer]), end='')
                case ',':
                    cell_arr[pointer] = ord((input()[0]))
                case '[':
                    if cell_arr[pointer] == 0:
                        codepos = stack[codepos]
                case ']':
                    if cell_arr[pointer] != 0:
                        codepos = stack[codepos]
            codepos += 1
        print(cell_arr)
    except:
        print(cell_arr)


def match_braces(s):
    temp, stack = [], {}
    for i, c in enumerate(s):
        if c == '[':
            temp.append(i)
        elif c == ']':
            start = temp.pop()
            stack[start] = i
            stack[i] = start
    return stack

def __main__():
    run(sys.argv[1])

__main__()