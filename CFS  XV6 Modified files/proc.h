// Segments in proc->gdt.
#define NSEGS     7

// Per-CPU state
struct cpu {
  uchar id;                    // Local APIC ID; index into cpus[] below
  struct context *scheduler;   // swtch() here to enter scheduler
  struct taskstate ts;         // Used by x86 to find stack for interrupt
  struct segdesc gdt[NSEGS];   // x86 global descriptor table
  volatile uint started;       // Has the CPU started?
  int ncli;                    // Depth of pushcli nesting.
  int intena;                  // Were interrupts enabled before pushcli?
  
  // Cpu-local storage variables; see below
  struct cpu *cpu;
  struct proc *proc;           // The currently-running process.
};

extern struct cpu cpus[NCPU];
extern int ncpu;

// Per-CPU variables, holding pointers to the
// current cpu and to the current process.
// The asm suffix tells gcc to use "%gs:0" to refer to cpu
// and "%gs:4" to refer to proc.  seginit sets up the
// %gs segment register so that %gs refers to the memory
// holding those two variables in the local cpu's struct cpu.
// This is similar to how thread-local variables are implemented
// in thread libraries such as Linux pthreads.
extern struct cpu *cpu asm("%gs:0");       // &cpus[cpunum()]
extern struct proc *proc asm("%gs:4");     // cpus[cpunum()].proc

//PAGEBREAK: 17
// Saved registers for kernel context switches.
// Don't need to save all the segment registers (%cs, etc),
// because they are constant across kernel contexts.
// Don't need to save %eax, %ecx, %edx, because the
// x86 convention is that the caller has saved them.
// Contexts are stored at the bottom of the stack they
// describe; the stack pointer is the address of the context.
// The layout of the context matches the layout of the stack in swtch.S
// at the "Switch stacks" comment. Switch doesn't save eip explicitly,
// but it is on the stack and allocproc() manipulates it.
struct context {
  uint edi;
  uint esi;
  uint ebx;
  uint ebp;
  uint eip;
};

enum procstate { UNUSED, EMBRYO, SLEEPING, RUNNABLE, RUNNING, ZOMBIE };

//This enumerator will be used to determine the color of each process in the red-black tree
enum procColor {RED, BLACK};		


// Per-process state
struct proc {
  uint sz;                     // Size of process memory (bytes)
  pde_t* pgdir;                // Page table
  char *kstack;                // Bottom of kernel stack for this process
  enum procstate state;        // Process state
  int pid;                     // Process ID
  struct proc *parent;         // Parent process
  struct trapframe *tf;        // Trap frame for current syscall
  struct context *context;     // swtch() here to run process
  void *chan;                  // If non-zero, sleeping on chan
  int killed;                  // If non-zero, have been killed
  struct file *ofile[NOFILE];  // Open files
  struct inode *cwd;           // Current directory
  char name[16];               // Process name (debugging)

/*
  CFS Implementation:
  -The red-black tree data structure will be used as the run queue for the Completely Fair Scheduler to chose processes.
  -The CFS scheduler will determine which process to pick based on the virtual Runtime of a process.
  
*/
  int virtualRuntime;    	//Elapsed time since it was scheduled      
  int currentRuntime;		//Time the process has run			
  int maximumExecutiontime;	//The target scheduling latency of each process per scheduling round
  int niceValue;		//It is the variable that will be used to determine initial priority for process
  int weightValue;		//Variable used to determine the process's maximumExecutiontime 			

/*
  Red-Black Tree data structure:
  -Each process that is in the red-black tree will contain three pointers to processes that are the parent and two childern. 
  -If the currently inserted process in the red-black tree run queue is the root, i.e. the only process in the red-black tree or is the highest process in the red-black tree, then the parent pointer will point to itself, and the left/right pointers will point to either null or another process if it is in the tree.
  -Other processes in the red-black tree will have a pointer to a parent process, i.e those processes are children of a process closer to the root, and the left/right pointers either will point to NULL or processes.
  -Each pointer pointing to null will be considered as a black node on the true. 
  -A process in the red-black tree can be either red or black. 
*/

  enum procColor coloring;
  struct proc *left;
  struct proc *right;
  struct proc *parentP;
};

// Process memory is laid out contiguously, low addresses first:
//   text
//   original data and bss
//   fixed-size stack
//   expandable heap
