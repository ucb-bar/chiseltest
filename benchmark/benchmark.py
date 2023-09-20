#!/usr/bin/env python3
# Copyright 2023 The Regents of the University of California
# released under BSD 3-Clause License
# author: Kevin Laeufer <laeufer@cs.berkeley.edu>
import os
import sys
from pathlib import Path
import subprocess

# possible locations for Java 8
java_8_paths = [
    "/usr/lib/jvm/java-1.8.0/bin/java"
]

_script_dir = Path(__file__).parent.resolve()
jar_file = _script_dir / "benchmark.jar"
main_class = "fsim.Benchmark"
hprof = "-agentlib:hprof=cpu=samples,depth=100,interval=20,lineno=y,thread=y,file=out.hprof"




def get_jvm_major_version(java: str) -> int:
    r = subprocess.run([java, "-version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out = r.stdout.decode('utf-8').split('\n') + r.stderr.decode('utf-8').split('\n')
    needle = 'version "'
    for line in out:
        if needle in line:
            numbers = [int(n) for n in line.split(needle)[1].split('.')[0:2]]
            if numbers[0] == 1:
                return numbers[1]
            else:
                return numbers[0]
    raise RuntimeError("Unexpected version output:\n" + '\n'.join(out))

def try_to_find_java_8() -> str:
    for path in java_8_paths:
        path = Path(path).resolve()
        if path.exists() and path.is_file():
            version = get_jvm_major_version(str(path))
            if version == 8:
                return str(path)
            else:
                print(f"{path} points to Java {version}")
        else:
            print(f"{path} does not exist")
    return "java"


hprof_2_flamegraph = _script_dir / "hprof2flamegraph"
hprof_2_flamegraph_repo = "https://github.com/cykl/hprof2flamegraph.git"
def get_hprof_2_flamegraph() -> Path:
    if not hprof_2_flamegraph.exists():
        # clone
        cmd = ['git', 'clone', hprof_2_flamegraph_repo, str(hprof_2_flamegraph.resolve())]
        subprocess.run(cmd, check=True)
    assert hprof_2_flamegraph.exists()
    return hprof_2_flamegraph

def run_hprof_2_flamegraph(out: Path) -> Path:
    script = get_hprof_2_flamegraph() / "stackcollapse_hprof.py"
    collapsed = Path("out-folded.txt")
    with open(collapsed, 'wb') as ff:
        subprocess.run([str(script.resolve()), str(out.resolve())], stdout=ff, check=True)
    assert collapsed.exists()
    return collapsed

flamegraph_dir = _script_dir / "FlameGraph"
flamegraph_repo = "https://github.com/brendangregg/FlameGraph.git"
def get_flamegraph() -> Path:
    if not flamegraph_dir.exists():
        # clone
        cmd = ['git', 'clone', flamegraph_repo, str(flamegraph_dir.resolve())]
        subprocess.run(cmd, check=True)
    assert flamegraph_dir.exists()
    return flamegraph_dir

def run_flamegraph(collapsed: Path) -> Path:
    script = get_flamegraph() / "flamegraph.pl"
    svg = Path("out.svg")
    with open(svg, 'wb') as ff:
        subprocess.run([str(script.resolve()), str(collapsed.resolve())], stdout=ff, check=True)
    assert svg.exists()
    return svg


def convert_hprof_out(out: Path):
    assert out.exists(), f"{out} does not exists"
    collapsed = run_hprof_2_flamegraph(out)
    svg = run_flamegraph(collapsed)
    # cleanup
    os.remove(collapsed)
    os.remove(out)
    return svg

def main():
    assert jar_file.exists(), f"{jar_file} not found. Did you run sbt assembly?"

    do_profile = True

    java = try_to_find_java_8() if do_profile else "java"
    version = get_jvm_major_version(java)
    cmd = ["-cp", jar_file, main_class]
    if do_profile:
        if version == 8:
            cmd = [hprof] + cmd
        else:
            print(f"WARN: we currently only support profiling with JVM 8, but you are running {version}")

    subprocess.run([java] + cmd + sys.argv[1:])

    if do_profile and version == 8:
        svg = convert_hprof_out(Path("out.hprof"))
        print(f"Wrote flame graph to {svg}")



if __name__ == '__main__':
    main()