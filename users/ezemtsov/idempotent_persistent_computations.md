# Idempotent Persistent Computations with Nix

Resoptima has numerous idempotent persistent computations defined as embedded "tasks" in Python. Tasks are a part of application-specific libraries, and are deployed currently together as one Python web application. Purpose of such applications is to help users with different kinds of analysis on top of the original data, front-end for this analysis is defined in [Dash](https://github.com/plotly/dash).

Tasks sometimes run by the same python server process that is responsible for UI. But sometimes when tasks require more resources or benefit from parallelization they can be defined as kubernetes jobs. To orchestrate those remote k8s executions we use a framework called [Prefect](https://www.prefect.io/) which ensures that embedded python code is being sent to a remote pod started by k8s and executed asynchronously.

Below is an example of a typical Prefect flow that exists in Resoptima:

```Python
    @flow(task_runner=SequentialTaskRunner())
    def run_kalman_iterate_flow(json_config: str):
        json_config = json.loads(json_config)
        config_path = "/tmp/data_assimilation_config.json"
        with open(config_path, "w") as f:
            json.dump(json_config, f, indent=4)

        result = subprocess.run(
            [
                "Firm.DataAssimilation.Console",
                "iterate",
                "--config-file",
                config_path,
            ],
            capture_output=True,
            text=True,
        )

        if result.returncode != 0:
            raise Exception(f"Stdout: {result.stdout}; Stderr: {result.stderr}")
```

This code results is a remote k8s job run that starts .NET executable, and run Kalman Filter based on some input data (referred by the input configuration file).
Result of this run is a bunch of parquet files written to blob storage, accessible by the UI for visualization.

#### Hermeticity
Currently the function is not hermetic, during it's runtime it will try to fetch additional data from blob storage.
On success it will write a bunch of files, which must be available outside of the task run.

Luckily `Firm.DataAssimilation.Console` executable works with file system, so blob storage IO operation could be moved out into pre/post processing.

#### Purity
This specific task is not pure. Kalman Filter is an example of a stochastic function that uses gaussian noise as an input.

For this particular flow, reducing entropy and making output reproducible is not optimal as it reduces quality of the output.
Ideally we want to be able to easily re-trigger execution during debugging, but output inherently by nature of the task will never be reproducible.

#### Idempotency
Important part is despite output not being deterministic, computation is treated as idempotent, so that we never want run it twice with the same input.
As soon as output of the Kalman run is calculated and saved to blob storage, we don't want to recompute it, unless input data, input dependencies or actual logic of computation is changed.

## Problems

#### Complexity
Current solution is extremely complex to configure and debug.

On configuration side it involves having Prefect Orion server running in k8s cluster, that is responsible of jobs status tracking and copying of embedded python code into remote pod on startup. It also computes idempotency keys for every flow.

Jobs must be started with the correct container image having all required dependencies in advance, which quickly leads to bloated worker images having anything potentially needed for any potential tasks with sizes over 3-4 GB.

The fact that one needs complex infrastructure to run a simple test results in a very slow development feedback making this way of working extremely inefficient. To mitigate infrastructure issues, Prefect allows to run python code locally, but it doesn't guarantee that dependencies for local environment are same as during remote execution.

#### Language restrictions
Prefect only allows to define tasks in python code. When remote tasks defined in a different framework (like .NET in the example above), Prefect flow is then defined as a OS-level system call to a binary which is already available in the worker container image.

#### Data migration
Sometimes we find bugs in computation logic, and it's *very* hard to fix those. Since Prefect idempotency keys are not taking into account worker image, just inputs and embedded python code, it's not guaranteed to recompute files at correct trigger.


## Alternatives
#### Apache Spark
We considered using Apache Spark as an alternative to Prefect some time ago, but it doesn't seem to solve anything in this case, as it requires even more complexity in infrastructure setup and introduces even a bigger competence barrier for application developers.

## Possible solutions
#### Nix
Derivation model in nix allows to evaluate a hermetic task on one machine and schedule it on a arbitrary remote worker. Output of the derivation build could be collected asynchronously from shared `/nix/store` that could be available at blob storage. Using nix solves the problem of testing such tasks locally as remote builders have absolutely no domain logic and solve only resource distribution problem. This also solves the data migration issues since nix takes both code and build inputs for computing idempotency keys (store paths). We also solve issues with framework restrictions since nix is language agnostic.

##### Nix example:
Below is an example of the Kalman Filter task defined nix. Note that `config` variable would contain `/nix/store` pointers to source data.
```nix
{ self, pkgs, lib, ... }:

config:
  pkgs.runCommand "run_kalman_iterate_flow" {
    buildInputs = [ self.projects.firm.dataassimilation ];
  } ''
    export OUTPUT_PATH = $out;
    Firm.DataAssimilation.Console iterate --config-file ${config}
  ''
```

##### Python example:
Most interesting user experience would arguably is when source code is aware of the original repository structure that's used for development. So that if we take nix code example and imagine that we have a bindings to nix API, we could get:
```
from magrathea.types import NixPath
from magrathea import self, pkgs

def run_kalman_iterate_flow(config: NixPath):
    pkgs.runCommand(
        name = "run_kalman_iterate_flow",
        buildInputs = [ self.projects.firm.dataassimilation ],
        cmd = f"Firm.DataAssimilation.Console iterate --config-file {config}",
    )
```
