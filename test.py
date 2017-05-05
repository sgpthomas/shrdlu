from pwn import *
import re

test1 = [
    ("create a blue sphere", ["created 1 blue sphere"]),
    ("create an orange cube", ["created 1 orange cube"]),
    ("move the orange cube to the right of the blue sphere", ["Debug returnID: 1 such orange cubes ", "moved 1 orange cube to the right of the blue sphere"]),
    ("#print", [" [1] -> blue sphere ( right: 2 )", " [2] -> orange cube ( left: 1 )"])
]

self_reference = [
    ("create a red cube", ["created 1 red cube"]),
    ("move a red cube above the red cube", ["Debug returnID: 1 such red cubes ", "can't move an object in reference to itself"]),
    ("#print", [" [1] -> red cube ()"])
]

tests = [test1, self_reference]

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
        res = True
        for (c, r) in t:
            result = command(child, c).split('\n')
            result.pop()
            res = res & (result == r)
            if result == r:
                log.info("Passed")
            else:
                log.warning("Failed! Expected \"{}\" but got \"{}\"".format(r, result))

        results[i] = res
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

