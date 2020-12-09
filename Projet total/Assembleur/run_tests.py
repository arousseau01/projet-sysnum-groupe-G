import subprocess
import os

good_path = "tests/good/"
bad_path = "tests/bad/"
DEVNULL = open(os.devnull, 'w')

def test_good_files():
    print(">> Testing against GOOD files in %s" % good_path)
    score = 0
    total = 0
    for file in os.listdir(good_path):
        if os.path.isfile(good_path + file):
            total += 1
            status = subprocess.call(
                ["./minias", good_path + file], 
                stdout = DEVNULL, 
                stderr = subprocess.STDOUT)
            if status == 0:
                score += 1
            else:
                print("FAILED on %s (should pass)" % file)
    if total == 0:
        print("found no file to test in")
    else:
        print("score:%d/%d" % (score, total))

def test_bad_files():
    print(">> Testing against BAD files %s" % bad_path)
    score = 0
    total = 0
    for file in os.listdir(bad_path):
        if os.path.isfile(bad_path + file):
            total += 1
            status = subprocess.call(
                ["./minias", bad_path + file], 
                stdout = DEVNULL,
                stderr = subprocess.STDOUT)
            if status == 1:
                score += 1
            else:
                print("FAILED on %s (should reject)" % file)
    if total == 0:
        print("found no file to test")
    else:
        print("score:%d/%d" % (score, total))

if __name__ == "__main__":
    test_good_files()
    test_bad_files()
    

