# MJPdes

A discrete event simulation engine for multi-job production

Multi-job production systems are production systems where jobs of specified job types flow through all
workcenters of the main line, but the processing time of each job, depending on it job type, may differ.

The terminology used in the following follows that of [1]. 

The program currently enables modeling of asynchronous production lines consisting of machines separated by
non-zero sized buffers. Machine reliability is exponential (i.e. modeled as a time-continuous Markov chain
with states 'up' and 'down.') Machines specify a work capacity. 
Job types describe the amount of work required at each machine. The portion of each job type is specified
and jobs of those types are sent to the first machine in the line in a random sequence.

The program reports:
 * Starvation and blocking at each machine. (The first machine cannot starve; the last machine cannot be blocked.)
 * Throughput
 * The bottleneck machine
 * Observed residence time
 * Average buffer occupancy 

## References

[1] Jingshan Li, Semyon Meerkov, *Production Systems Engineering*, Springer, 2009. 

## Installation

1. Download the [leiningen shell script](http://leiningen.org/).
2. Type 'lein deps' at a shell prompt in the MJPdes directory and wait while it downloads 
    all the related libraries. 
3. Type 'lein bin' at a shell prompt in the MJPdes directory. This will build an executable in the MJPdes/target directory. 

## Usage

The program simply takes a model specification as a file and runs it, printing results at the shell where it was started.
Depending on the parameters, execution may take a few minutes or more. There is an example input file in the
resources subdirectory.

## Limitations

Currently:
 * The program only simulates a single line (no subassembly lines).
 * Buffer size must be > 0.
 * The program only supports exponential machine models.

''''
cd MJPdes/target

target> ./MJPdes ../resources/example.clj

{:TP 0.8436111,
 :computed-residence-time 11.287120820692454,
 :wip
 {:b1 2.64061657672258,
  :b2 2.8155548730213145,
  :b3 0.6295534023826856,
  :b4 0.1731391498607996},
 :number-of-jobs 15185,
 :status nil,
 :runtime 18000,
 :bneck :m2,
 :starved
 {:m1 0.0,
  :m2 0.007273298040929123,
  :m3 0.02995790236812948,
  :m4 0.06732870121928423,
  :m5 0.20629909291718984},
 :observed-residence-time 12.943294452883347,
 :blocked
 {:m1 0.10366188424439529,
  :m2 0.019637373044097253,
  :m3 0.05516286216553973,
  :m4 0.01876798318977487,
  :m5 0.0}}
''''

The input file is a Clojure map. A description of the  


## Disclaimer
The use of any software or hardware by the project does not imply a recommendation or endorsement by NIST.

The use of the project results in other software or hardware products does not imply a recommendation or endorsement by NIST of those products.

We would appreciate acknowledgement if any of the project results are used, however, the use of the NIST logo is not allowed.

NIST-developed software is provided by NIST as a public service. You may use, copy and distribute copies of the software in any medium, provided that you keep intact this entire notice. You may improve, modify and create derivative works of the software or any portion of the software, and you may copy and distribute such modifications or works. Modified works should carry a notice stating that you changed the software and should note the date and nature of any such change. Please explicitly acknowledge the National Institute of Standards and Technology as the source of the software.

NIST-developed software is expressly provided “AS IS.” NIST MAKES NO WARRANTY OF ANY KIND, EXPRESS, IMPLIED, IN FACT OR ARISING BY OPERATION OF LAW, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT AND DATA ACCURACY. NIST NEITHER REPRESENTS NOR WARRANTS THAT THE OPERATION OF THE SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE, OR THAT ANY DEFECTS WILL BE CORRECTED. NIST DOES NOT WARRANT OR MAKE ANY REPRESENTATIONS REGARDING THE USE OF THE SOFTWARE OR THE RESULTS THEREOF, INCLUDING BUT NOT LIMITED TO THE CORRECTNESS, ACCURACY, RELIABILITY, OR USEFULNESS OF THE SOFTWARE.

You are solely responsible for determining the appropriateness of using and distributing the software and you assume all risks associated with its use, including but not limited to the risks and costs of program errors, compliance with applicable laws, damage to or loss of data, programs or equipment, and the unavailability or interruption of operation. This software is not intended to be used in any situation where a failure could cause risk of injury or damage to property. The software developed by NIST employees is not subject to copyright protection within the United States.




