from py_nix_compat import PyDerivation
import os.path
from pathlib import Path

def read_derivation(path):
    with open(path, 'r') as f:
        return PyDerivation.from_aterm_bytes(f.read().encode('utf8'))

def chase(base_divergence_dir, golden, diverging):
    for key, value in diverging.environment.items():
        # Of course, it diverges in out!
        if key == 'out':
            continue
        golden_value = golden.environment[key]
        if golden_value != value:
            print(f'Diverges in {key}!')
            value = value.decode('utf8')
            golden_value = golden_value.decode('utf8')

            if key == 'buildInputs':
                diverging_build_inputs = set(value.split(' '))
                golden_build_inputs = set(golden_value.split(' '))
                missing_build_inputs = golden_build_inputs - diverging_build_inputs
                extraneous_build_inputs = diverging_build_inputs - golden_build_inputs

                print('missing', missing_build_inputs)
                print('extraneous', extraneous_build_inputs)

                for missing, extra in zip(missing_build_inputs, extraneous_build_inputs):
                    chase(base_divergence_dir,
                          read_derivation(missing),
                          read_derivation(base_divergence_dir / os.path.basename(extra)))


golden_drv = read_derivation("/nix/store/rnnpslz4pvmcdwml894l5vqrsjcv86yc-acl2-8.5.drv")
diverging_drv = read_derivation("/tmp/tvix/.tmpJNScxE/h9y85y7d373v5xvsa2ml37nfhw5440q3-acl2-8.5.drv")
print(diverging_drv.input_derivations)
chase(Path("/tmp/tvix/.tmpJNScxE"), golden_drv, diverging_drv)
