-----------------------------------------------------------------------------------------------
# Kronos

The ECLAT [stock service](services.md#stock) __Kronos__ executes _jobs_ in the background and 
controls them. 

The expression 'in the background' means (in this context, at least) 'executing in parallel 
with other software'. 

A job encapsulates something that needs to be done, but which doesn't itself need any (direct) 
user interaction. 

Programs that use AdaOS should make use of Kronos, perhaps extensively, for running background
tasks, rather than using tasks and having to implement the functionality that Kronos provides. 

.....

On [hosted platforms](../pxcr/targets.md#plat), all services run within the execution of the
executable file. 

On the AdaOS Native platform, Kronos jobs execute _sessionless_, meaning that they execute
regardless of which users are logged in (or where) or indeed whether anyone is logged in at
all. 

There are various different kinds of job, as described in the next section. 



-----------------------------------------------------------------------------------------------
## Job Types {#types}

There are different types of job, .....

 * run a program; 

 * perform operations on one or more files (copy, move/rename, delete); 

 * transfer data across the network; 

 * print a document; 

 * perform a process such as compiling a set of source files or rendering a video; 
 
 * perform a sequence of other jobs, one after the other; 
 
 * perform a set of other jobs, all in parallel with each other; 
 
 * .....

.....





-----------------------------------------------------------------------------------------------
## Job States {#states}

A specific job is represented by a _system job_ object, which is a [system
object](../objects/objects.md) of a type derived from the limited private type `System_Job`,
which is declared in the package `AdaOS.Jobs`. 

A system job object encapsulates all of the state of a specific job, prior, during, and after
its execution (some jobs execute multiple times). This state is the [saved
state](../objects/objects.md#state) of the system job object. 

.....

Every job state contains at least:

 * the [job status](#status); 
 
 * a pointer to a [job trigger](#triggers); 
 
 * an execution [history](#history); 

 * any other data the job needs to execute. 
 
.....



-----------------------------------------------------------------------------------------------
## Job Status {#basic}

Every system job has a _job status_, which is an enumeration type that indicates if the job:

 * is still being set up, and is not ready to be executed (`Setup`); 
 
 * is ready to be executed but has not yet actually started (`Ready`); 
 
 * is currently being executed (`Busy`); 
 
 * has been finally completed, and is not ready to be executed again, but is ready to be 
   [derostered](#rost) (`Finished`). 

.....



-----------------------------------------------------------------------------------------------
## Execution History (#history)

.....





-----------------------------------------------------------------------------------------------
## Job Types {#type}

.....



-----------------------------------------------------------------------------------------------
## 








-----------------------------------------------------------------------------------------------
## Helpers {#helpers}

Jobs are directly executed by Kronos, but the implementations of different types of job are 
provided by different _job helpers_. 

A job helper is a [plugin](../pxcr/plugins.md), in the module class 

?????`system.jobs.helper`, .....


### File Operations {#fileops}

.....


### Future Job Types

 * .....




-----------------------------------------------------------------------------------------------
## Triggers {#trig}

A _job trigger_ is an object that describes when the execution of a job should begin.

.....

?????Every system job object contains a pointer to a trigger. That trigger determines when the job 
will be executed. 

When a job is [rostered](#rost), both a system job object and a job trigger object cited, .....

.....


### Time Triggers

A _time trigger_ is a job trigger that schedules the execution of the job 
at a specific date and time. 

Kronos maintains a register of _time triggers_. 

Each time trigger object ......


### Repetition Triggers

A _repetition trigger_ is a job trigger that schedules the execution of the job 
to occur repeatedly, with a specific interval between each repetition. 

.....




-----------------------------------------------------------------------------------------------
## Job Execution

The type `System_Job` has an abstract primitive operation, a procedure, named `Execute`. 

Calling this procedure executes the job. 

.....




-----------------------------------------------------------------------------------------------
## Progress {#prog}

The `Execute` procedure of jobs is passed a parameter, named `Updater`, which is a _progress 
updater_, and is of a type in `Progress_Updater'Class`. The interface type `Progress_Updater` 
is declared in `AdaOS.Jobs`. 

A progress updater is an object that enables the `Execute` procedure to keep the world informed 
of its progress as it executes. 

To do this, the body of the `Execute` procedure can (and generally should) make frequent calls 
of the operations of the updater object passed into it. 

.....

The execution of a job is assumed to comprise a sequence of _job phases_. Within the execution
of a job, each phase has a different name to identify it. 

It is assumed that, in general, the phases execute in parallel with each other. However, it is
not a problem if there is some sequentiality to their execution. 

Of course, a job might only have one phase. 

Each phase is assumed to comprise a sequence of _phase steps_. It is assumed that, within a
phase, a step does not begin until the previous step has been completed. Ideally the amount of
time taken to complete for each step is approximately equal, within a phase. 

Steps within a phase are identified by an integer starting at 1. Each phase has a certain
number of steps, which does not change, but each phase has a _current step_, which changes as
progress is made through the phase. When the phase is completed, its current step becomes one
greater than the number of steps in the phase. 

Again, a phase might have only one step. 

A _job phase descriptor_ is an object, of the type `Job_Phase` declared in `AdaOS.Jobs`, that 
contains: 

 * the name of the phase; 
 
 * the number of steps in the phase.

The `Start` procedure signifies the beginning of execution of the job. It should be called
once, as early as possible in the execution of the job. It takes one parameter, named `Phases`,
which is an array of _job phase descriptors_. The array is indexed by _phase number_, starting
at 1.

The current step of each phase is set to 1, indicating that the phase is executing the first of
its steps. 

The `Step` procedure should be called whenever one or more steps have been completed during the
execution of a phase. This procedure takes two parameters: a positive integer named `Phase`,
which is the phase number of a phase; a positive integer named `Steps`, which should be set to
the number of further steps which have been completed in the given phase. `Steps` has a default
value of 1. 

This procedure should be called as often as possible, but not when no more steps have been
completed since the last time it was called. It cannot go backwards (reducing the number of
steps completed since the last time it was called), and it should not report more steps than
the total number of steps passed into the `Start` procedure. 

.....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Job Roster {#rost}

Kronos maintains a list of jobs, called its _roster_. 

.....




-----------------------------------------------------------------------------------------------
## Events

Kronos sends [events](../events/events.md) for many of its activities:

 * receiving a job into its roster; 
 
 * .....
 
 * beginning the execution of a job; 
 
 * starting a phase during the execution of a job; 
 
 * .....







the package `Kronos.Events`. 






-----------------------------------------------------------------------------------------------
## Program Execution Jobs

A _program runner_ executes a [program](../rts/programs.md). 

Kronos has a built-in implementation of the job type `Run_Program_Job` ......








-----------------------------------------------------------------------------------------------
## Job Sequences

Kronos has a built-in implementation of the job type `Job_Sequence_Job` ......








-----------------------------------------------------------------------------------------------
## Job Batteries

Kronos has a built-in implementation of the job type `Job_Battery_Job` ......

A _job battery_ executes a set of other jobs in parallel. 






-----------------------------------------------------------------------------------------------
## Configuration

.....

The name of the Kronos service is `kronos`; the name of the Kronos service program is `kronos`,
which is made from one module, named `kronos`, plus many plugins. 

.....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Example

.....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 






