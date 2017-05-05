from pwn import *
import re

test1 = {
    "create a red cube": ["created 1: red cube"],
    "create a blue sphere above the red cube": ["created 2: blue sphere"],
    "#print": [" [1] -> red cube ( above: 2 )"," [2] -> blue sphere ( below: 1 )"]
}

test2 = {
    "create a blue cube": ["created 1: blue cube"]
}

tests = [test1, test2]

ansi_escape = re.compile(r'\x1b[^m]*m')

def new_child():
    return process(["./shrdlu"])

def command(p, cmd):
    p.sendline(cmd)
    resp = p.recvuntil("shrdlu>")
    return ansi_escape.sub('', resp.strip())

def run_tests():
    results = {}
    for i, t in enumerate(tests):
        log.info("*=*=*=* Running  Test {} *=*=*=*".format(i))
        child = new_child()
        child.recvuntil("shrdlu>")
        for s in t:
            result = command(child, s).split('\n')
            result.pop()
            results[i] = (result == t[s])
            if result == t[s]:
                log.info("Passed")
            else:
                log.warning("Failed! Expected \"{}\" but got \"{}\"".format(t[s], result))

        child.kill()
        log.info("*=*=*=* Finished Test {} *=*=*=*".format(i))
        print("")

    for i in results:
        if results[i]:
            log.info("Passed Test {}".format(i))
        else:
            log.warning("Failed Test {}".format(i))

if __name__ == "__main__":
    run_tests()

