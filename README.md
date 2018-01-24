# MJPdes

A discrete event simulation engine for mixed-model asynchronous serial production lines

This software supports simulation of mixed-model and multi-job production systems.
Multi-job production is a specialization of mixed-model production systems, are production systems
where jobs of specified job types flow through all workcenters (machines) of the main line, but the
processing time at each workcenter may differ according to the job's type.

The terminology used on this page mostly follows that of reference [1]. 

The program currently enables modeling of asynchronous production lines consisting of machines separated by
non-zero-sized buffers. Machine reliability is exponential (i.e. modeled as a continuous-time
Markov chain with Poisson processes of 'up' and 'down' events). Machines specify a work capacity. 
Job types describe the amount of work required at each machine. The portion of each job type to
be produced is specified and jobs of those types are sent to the first machine in the line in a
random sequence.

The program reports:
 * Starvation and blocking at each machine. (The first machine cannot starve; the last machine cannot be blocked.)
 * Throughput
 * The bottleneck machine
 * Observed residence time
 * Average buffer occupancy
 * A 'SCADA log' of events that occurred

## References

[1] Jingshan Li, Semyon Meerkov, *Production Systems Engineering*, Springer, 2009. 

## Installation

1. Download the [leiningen shell script](http://leiningen.org/).
2. Type 'lein deps' at a shell prompt in the MJPdes directory (where you cloned this git repository) and wait while
   leiningen downloads all the related libraries. 
3. Type 'lein bin' at a shell prompt in the MJPdes directory.
This will build an executable in the MJPdes/target directory. 

## Usage

The program takes a model specification as a file and runs it, printing results at the shell where it was started
or to a file. Depending on the parameters, execution may take a few seconds or a few minutes or more. There is an
example input file in the resources subdirectory.

```bash
cd MJPdes/target

target> ./MJPdes -i ../resources/example.clj
```
Using the command line switch -o, you can specify an output file to which results are written.
This is especially useful when you'd like to collect details about jobs moving through the system.
(See the discussion below regarding use of the key/value pair for the :report option
to learn how to get this detailed output.)

```bash
cd MJPdes/target

target> ./MJPdes -i ../resources/example.clj -o /tmp/output.clj
````

### Typical output

```clojure
{:TP 0.8436111,
 :average-buffer-occupancy
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
```

The input file is a essentially a Clojure map. The syntax of Clojure maps follows
this pattern:

```clojure
{:akey "a value" :another-key {:key3 "a value in an nested map"}}
```

A detailed description can be found [here](https://github.com/edn-format/edn).

Comments in Clojure start with a semicolon. Commas are treated as whitespace. 

An annotation of the example input file follows:

```clojure
(map->Model  ; a function call to make a Model from the following map argument
 {:line ; a key introducing the line
  {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })  ; the definition of machine m1
   :b1 (map->Buffer {:N 3})                              ; the definiton of buffer b1
   :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
   :b2 (map->Buffer {:N 5})
   :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })  ; lambda is breakdown rate.
   :b3 (map->Buffer {:N 1})                              ; N is buffer size.
   :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.05 }) ; mu is repair rate.
   :b4 (map->Buffer {:N 1})
   :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })}
  :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]       ; the arrangement of the line
  :entry-point :m1                                      ; the entry point to the line
  :params {:warm-up-time 2000 :run-to-time 20000}  ; data is collected from time 2000-20000
  :jobmix {:jobType1 (map->JobType {:portion 0.8 ; 80% of jobs will be of type jobType1.
                                    :w {:m1 1.0, :m2 1.0, :m3 1.0, :m4 1.0, :m5 1.0}})
           :jobType2 (map->JobType {:portion 0.2 ; 20% of jobs will be of type jobType2.
                                    :w {:m1 1.0, :m2 2.0, :m3 1.5, :m4 1.0, :m5 1.0}})}})
```

The ExpoMachine, Buffer and JobType forms create objects of the type named.
The details of these objects are as follows:

```clojure
(map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })
```
* :lambda is the breakdown rate.
* :mu is the repair rate.
* :W is the work capacity.

```clojure
(map->Buffer {:N 3})
```
* :N is the size of the buffer.

```clojure
(map->JobType {:portion 0.2 ; 20% of jobs will be of type jobType2.
               :w {:m1 1.0, :m2 2.0, :m3 1.5, :m4 1.0, :m5 1.0}})
```
* :portion determines the percentage of jobs to be processed that will be of this type.
* :w introduces a nested map of the work requirements at each machine. e.g. 2.0 units
   of work at machine :m2.

## Detailed 'SCADA log' output with :report

You can get detailed output from the program describing (1) machine state changes
including starting and completing jobs, starvation and blocking, (2) movement of jobs
from machines to buffers and (3) machine state changes (up/down). These reporting
features are controlled individually using a map of key/value pairs, :report, as shown
below. This map should be placed at the same level of parentheses nesting
as the other application keys (i.e. placed with :topology, :job-mix etc.).

If :report is not specified, results are limited to those described earlier.
When :report is specified, it is best to direct output to a file using
the -o flag described earlier. 

An example report specification:

```clojure
:report {:max-lines 1000 :job-detail? true :machine-up&down? true}
```

We first describe basic output. The three directives :max-lines, :job-detail? and :machine-up&down?
are then described in turn.

### Basic Output

Specifying :report {:max-lines <some number>} will produce output similar to the following:

```clojure
{:clk 1999.8512 :act :m1-blocked :m :m1 :mjpact :bl :line 0}
{:clk 2000.0861 :act :m2-move-off :m :m2 :ent 1994.8512 :mjpact :ej :j 1597 :line 1}
{:clk 2000.0861 :act :m2-start-job :m :m2 :bf :b1 :n 3 :mjpact :sm :j 1598 :line 2}
{:clk 2000.0861 :act :m1-unblocked :m :m1 :mjpact :ub :line 3}
{:clk 2000.0861 :act :m1-move-off :m :m1 :bf :b1 :n 2 :mjpact :bj :j 1601 :line 4}
{:clk 2000.0861 :act :m1-start-job :jt :jobType1 :m :m1 :ends 2001.0861 :mjpact :aj :j 1602 :line 5}
{:clk 2001.0861 :act :m1-blocked :m :m1 :mjpact :bl :line 6}
{:clk 2001.1861 :act :m2-move-off :m :m2 :ent 1995.8512 :mjpact :ej :j 1598 :line 7}
{:clk 2001.1861 :act :m2-start-job :m :m2 :bf :b1 :n 3 :mjpact :sm :j 1599 :line 8}
{:clk 2001.1861 :act :m1-unblocked :m :m1 :mjpact :ub :line 9}
{:clk 2001.1861 :act :m1-move-off :m :m1 :bf :b1 :n 2 :mjpact :bj :j 1602 :line 10}
{:clk 2001.1861 :act :m1-start-job :jt :jobType1 :m :m1 :ends 2002.1861 :mjpact :aj :j 1603 :line 11}
{:clk 2002.1861 :act :m1-blocked :m :m1 :mjpact :bl :line 12}
{:clk 2002.2861 :act :m2-move-off :m :m2 :ent 1996.8512 :mjpact :ej :j 1599 :line 13}
```
Note that:
* :n is the occupancy of the buffer before the action (:act).
* :ent is the time at which the job enters the system and starts work on the first machine.
* :ends is the time at which the job will end.
* :m is the machine involved (if any).
* :bf is the buffer involved (if any).
* :jt is job type.
* :j is the job id. 
* :mjpact is a shorthand classification of what is happening.

The :mjpact values have the following meanings:
 - :sm = start on machine
 - :aj = add job (same as :sm but on entry machine).
 - :bl = blocking
 - :ub = unblocked
 - :st = starving
 - :us = unstarved

:max-lines specifies how many log entries (lines {:clk ...}) to print. Printing starts
after warm-up. (See :warm-up-time above.). 

### :job-detail?

Setting :job-detail? to true directs the program to output details of where each job
is in the system. An example log line with this data is:

{:clk 1999.8745 :act :m3-complete-job :m :m3 :mjpact :ej :ent 1990.8793 :j 1307
 :dets {:run {:m1 1313, :m2 1310, :m3 1307}, :bufs {:b1 [1311 1312], :b2 [1308 1309]}} :line 0}

:run indicates which jobs are currently running. In the example, 1313 is running on machine :m1.
:bufs describes what jobs are in buffers. The buffer downstream of machine :m1, :b1, contains
two jobs, 1311 and 1312.

### :machine-up&down?

Setting :machine-up&down? to true directs the program to report when an (exponential) machine
goes down or up. For example:

```clojure
{:clk 2032.0994 :act :m2-down :m :m2 :mjpact :down :line 98}
{:clk 2032.3566 :act :m2-up :m :m2 :mjpact :up :line 99}
```
In the example, at 2032.0994 machine :m2 went down; at 2032.3566 it came back up. 

## Limitations

Currently:
 * The program only simulates a single line (no subassembly lines).
 * Buffer size must be > 0.
 * The program only supports exponential and reliable machine models.

## Disclaimer
The use of any software or hardware by the project does not imply a recommendation or endorsement by NIST.

The use of the project results in other software or hardware products does not imply a recommendation or endorsement by NIST of those products.

We would appreciate acknowledgement if any of the project results are used, however, the use of the NIST logo is not allowed.

NIST-developed software is provided by NIST as a public service. You may use, copy and distribute copies of the software in any medium, provided that you keep intact this entire notice. You may improve, modify and create derivative works of the software or any portion of the software, and you may copy and distribute such modifications or works. Modified works should carry a notice stating that you changed the software and should note the date and nature of any such change. Please explicitly acknowledge the National Institute of Standards and Technology as the source of the software.

NIST-developed software is expressly provided “AS IS.” NIST MAKES NO WARRANTY OF ANY KIND, EXPRESS, IMPLIED, IN FACT OR ARISING BY OPERATION OF LAW, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT AND DATA ACCURACY. NIST NEITHER REPRESENTS NOR WARRANTS THAT THE OPERATION OF THE SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE, OR THAT ANY DEFECTS WILL BE CORRECTED. NIST DOES NOT WARRANT OR MAKE ANY REPRESENTATIONS REGARDING THE USE OF THE SOFTWARE OR THE RESULTS THEREOF, INCLUDING BUT NOT LIMITED TO THE CORRECTNESS, ACCURACY, RELIABILITY, OR USEFULNESS OF THE SOFTWARE.

You are solely responsible for determining the appropriateness of using and distributing the software and you assume all risks associated with its use, including but not limited to the risks and costs of program errors, compliance with applicable laws, damage to or loss of data, programs or equipment, and the unavailability or interruption of operation. This software is not intended to be used in any situation where a failure could cause risk of injury or damage to property. The software developed by NIST employees is not subject to copyright protection within the United States.




