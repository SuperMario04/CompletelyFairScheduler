#include "types.h"
#include "defs.h"
#include "param.h"
#include "memlayout.h"
#include "mmu.h"
#include "x86.h"
#include "proc.h"
#include "spinlock.h"

struct {
  struct spinlock lock;
  struct proc proc[NPROC];
} ptable;

//Red-Black Tree data structure
struct redblackTree {
  int count;
  int rbTreeWeight;
  struct proc *root;
  struct proc *min_vRuntime;
  struct spinlock lock;
  int period;
}rbTree; 

static struct proc *initproc;

static struct redblackTree *runnableTasks = &rbTree;

//Set target scheduler latency and minimum granularity constants
//Latency must be multiples of min_granularity
static int latency = NPROC / 2;
static int min_granularity = 2;

int nextpid = 1;
extern void forkret(void);
extern void trapret(void);

static void wakeup1(void *chan);

////////Red Black Tree functions for operations of Insertion and retrieving, while maintaining Red Black Tree properties

/*
  rbinit(struct redblackTree*, char)
  parameters: pointer that contains the address of the red-black tree and a string containing the name of the lock
  returns: none
  This function will initialize the red black tree data structure.
*/
void
rbinit(struct redblackTree *tree, char *lockName)
{
  initlock(&tree->lock, lockName);
  tree->count = 0;
  tree->root = 0;
  tree->rbTreeWeight = 0;
  tree->min_vRuntime = 0;

  //Initially set time slice factor for all processes
  tree->period = latency;
}

/*
  calculateWeight(int)
  parameters: the process's niceValue
  returns: an integer that signifies the weight of the process the address points to.
  This function will calculate each individual process's weight in respect to it's nice value.

  //-Nice value can be in between -20 to 19 in the linux kernel, but for our xv6 implementation we will use the range 0 to 30
  The default nice value for a process is set to 0

  The formula to determine weight of process is:
  1024/(1.25 ^ nice value of process)
*/
int
calculateWeight(int nice){

  double denominator = 1.25;

  //In order to ensure correct utilization of process priority during the time slice calculation
  //If a process has a higher nice value given, then for the formula to accurately utlize the priority level without losing precision
  //due to fraction casted to an int, it will give it a default value that will represent the same priority level in the system.
  if(nice > 30){
	nice = 30;
  }
  
  //While loop to calculate (1.25 ^ nice value) for denominator of formula to find weight. 
  int iterator = 0;
  while (iterator < nice && nice > 0){
  	denominator = denominator * 1.25;
  }

  return (int) (1024/denominator);
}

/*
  emptyTree(struct redblackTree*)
  parameters: pointer that contains the address of the red-black tree structure
  returns: none
  This function will determine if the tree is empty or not, i.e the tree has no processes in it.
*/
int
emptyTree(struct redblackTree *tree)
{
  return tree->count == 0;
}

/*
  fullTree(struct redblackTree*)
  parameters: pointer that contains the address of the red-black tree structure
  returns: none
  This function will determine if the tree is full, i.e the maximum alloted number of processes in the system
*/
int
fullTree(struct redblackTree *tree)
{
  return tree->count == NPROC;
}

//This two process retrieval functions will retrive the grandparent or uncle process of the process passed into the functions. This is done to preserve red black tree properties by altering states and positions of the tree.
struct proc*
retrieveGrandparentproc(struct proc* process){
  if(process != 0 && process->parentP != 0){
	return process->parentP->parentP;
  } 
	
  return 0;
}

struct proc*
retrieveUncleproc(struct proc* process){
  struct proc* grandParent = retrieveGrandparentproc(process);
  if(grandParent != 0){
	if(process->parentP == grandParent->left){
		return grandParent->right;
	} else {
		return grandParent->left;
	}
  }
	
  return 0;
}


/*
  rotateLeft(struct redblackTree*, struct proc*)
  parameters:The red black tree pointer to access and modify the root, and the current process in the tree that will be rotated to the left
  returns:none
  This function will perform a rotation on the process structure in the tree that is passed into the function. 
  It will perform a left rotation, where it will move down leftward in the tree and its right process will be moved up to its place.
*/
void 
rotateLeft(struct redblackTree* tree, struct proc* positionProc){
  struct proc* save_right_Proc = positionProc->right;
	
  positionProc->right = save_right_Proc->left;
  if(save_right_Proc->left != 0)
	save_right_Proc->left->parentP = positionProc;
  save_right_Proc->parentP = positionProc->parentP;
	
  if(positionProc->parentP == 0){
	tree->root = save_right_Proc;
  } else if(positionProc == positionProc->parentP->left){
	positionProc->parentP->left = save_right_Proc;
  } else {
	positionProc->parentP->right = save_right_Proc;
  }
  save_right_Proc->left = positionProc;
  positionProc->parentP = save_right_Proc;
}

/*
  rotateRight(struct redblackTree*, struct proc*)
  parameters:The red black tree pointer to access and modify the root, and the current process in the tree that will be rotated to the right
  returns:none
  This function will perform a rotation on the process structure in the tree that is passed into the function. 
  It will perform a right rotation, where it will move down rightward in the tree and its left process will be moved up to its place.
*/
void 
rotateRight(struct redblackTree* tree, struct proc* positionProc){
	
  struct proc* save_left_Proc = positionProc->left;
	
  positionProc->left = save_left_Proc->right;
	
  //Determine parents for the process being rotated
  if(save_left_Proc->right != 0)
	save_left_Proc->right->parentP = positionProc;
  save_left_Proc->parentP = positionProc->parentP;
  if(positionProc->parentP == 0){
	tree->root = save_left_Proc;
  } else if(positionProc == positionProc->parentP->right){
	positionProc->parentP->right = save_left_Proc;
  } else {
	positionProc->parentP->left = save_left_Proc;
  }
  save_left_Proc->right = positionProc;
  positionProc->parentP = save_left_Proc;
	
}

/*
  setMinimumVRuntimeproc(struct proc*)
  parameters: the address of a process in the tree to be utilized to traverse the tree
  returns:A pointer that contains the address to the process with the smallest Virtual Runtime
  This function will return a pointer to the address of the process with the smallest Virtual Runtime. 
  It will do this by traversing through the left branch of the tree until it reaches the process.
*/
struct proc*
setMinimumVRuntimeproc(struct proc* traversingProcess){
	
  if(traversingProcess != 0){
	if(traversingProcess->left != 0){
	    return setMinimumVRuntimeproc(traversingProcess->left);
	} else {
	    return traversingProcess;
	}
  }
	return 0;
}

struct proc*
insertproc(struct proc* traversingProcess, struct proc* insertingProcess){
	
  insertingProcess->coloring = RED;
	
  //i.e it is root or at leaf of tree
  if(traversingProcess == 0){
	return insertingProcess;
  }		
  //i.e everything after root
  //move process to the right of the current subtree
  if(traversingProcess->virtualRuntime <= insertingProcess->virtualRuntime){
	insertingProcess->parentP = traversingProcess;
	traversingProcess->right = insertproc(traversingProcess->right, insertingProcess);
  } else {
	insertingProcess->parentP = traversingProcess;		
	traversingProcess->left = insertproc(traversingProcess->left, insertingProcess);
  }
	
  return traversingProcess;
}

/*
  insertionCases(struct redblackTree*, struct proc*, int)
  parameters: the pointer of the tree, process in the red black tree and an integer value
  returns: none
  This function will contain different cases that will incorporate the properties for a red black tree. It will utilize the integer value to determine which case need to be handled.
  cases:
  -1: if the current inserted process is the root
  -2: if the current inserted process's parent is black
  -3: if both parent and uncle processes are red, then repaint them black
  -4: if parent is red and uncle is black, but current process is red and the current process is right child of parent that is left of grandparent or vice versa
  -5: same as case four but the current process is left child of parent that is left of grandparent or vice versa
*/
void
insertionCases(struct redblackTree* tree, struct proc* rbProcess, int cases){
	
  struct proc* uncle;
  struct proc* grandparent;
	
  switch(cases){
  case 1:
	if(rbProcess->parentP == 0)
		rbProcess->coloring = BLACK;
	else
		insertionCases(tree, rbProcess, 2);
	break;
	
  case 2:
	if(rbProcess->parentP->coloring == RED)
		insertionCases(tree, rbProcess, 3);
	break;
	
  case 3:
	uncle = retrieveUncleproc(rbProcess);
	
	if(uncle != 0 && uncle->coloring == RED){
		rbProcess->parentP->coloring = BLACK;
		uncle->coloring = BLACK;
		grandparent = retrieveGrandparentproc(rbProcess);
		grandparent->coloring = RED;
		insertionCases(tree, grandparent, 1);
		grandparent = 0;
	} else {
		insertionCases(tree, rbProcess,4);
	}
	
	uncle = 0;
	break;
  
  case 4:
	grandparent = retrieveGrandparentproc(rbProcess);
	
	if(rbProcess == rbProcess->parentP->right && rbProcess->parentP == grandparent->left){
		rotateLeft(tree, rbProcess->parentP);
		rbProcess = rbProcess->left;
	} else if(rbProcess == rbProcess->parentP->left && rbProcess->parentP == grandparent->right){
		rotateRight(tree, rbProcess->parentP);
		rbProcess = rbProcess->right;
	}
	insertionCases(tree, rbProcess, 5);
	grandparent = 0;
	break;
	
  case 5:
    grandparent = retrieveGrandparentproc(rbProcess);
	
	if(grandparent != 0){
		grandparent->coloring = RED;
		rbProcess->parentP->coloring = BLACK;
		if(rbProcess == rbProcess->parentP->left && rbProcess->parentP == grandparent->left){
			rotateRight(tree, grandparent);
		} else if(rbProcess == rbProcess->parentP->right && rbProcess->parentP == grandparent->right){
			rotateLeft(tree, grandparent);
		}
	}
	
	grandparent = 0;
	break;
	
  default:
	break;
  }
  return;
}

void
insertProcess(struct redblackTree* tree, struct proc* p){

  acquire(&tree->lock);
  if(!fullTree(tree)){	
	//actually insert process into tree
	tree->root = insertproc(tree->root, p);
	if(tree->count == 0)
		tree->root->parentP = 0;
    	tree->count += 1;
	
	//Calculate process weight
	p->weightValue = calculateWeight(p->niceValue);

	//perform total weight calculation 
	tree->rbTreeWeight += p->weightValue;
	
    	//Check for possible cases for Red Black tree property violations
	insertionCases(tree, p, 1);
		
	//This function call will find the process with the smallest vRuntime, unless 
	//there was no insertion of a process that has a smaller minimum virtual runtime then the process that is being pointed by min_vRuntime
	if(tree->min_vRuntime == 0 || tree->min_vRuntime->left != 0)
		tree->min_vRuntime = setMinimumVRuntimeproc(tree->root);
	 
  }	
  release(&tree->lock);
}

/*
  retrievingCases(struct redblackTree*, struct proc*, struct proc*, int)
  paramters: The red black tree pointer to access and modify the root, the parent of the process, the pointer to the process with the smallest virtual Runtime and the case number
  returns: none
  This function will check for violations of the red black tree to ensure the trees properties are not broken when we remove the process out of the tree. 
  cases:
  -1:We remove the process that needs to be retrieved and ensure that either the process or the process's child is red, but not both of them.
  -2:if both the process we want to remove is black and it has child that is black, then we would have to perform recoloring and rotations to ensure red black tree property is met.
  
*/
void
retrievingCases(struct redblackTree* tree, struct proc* parentProc, struct proc* process, int cases){
  struct proc* parentProcess;
  struct proc* childProcess;
  struct proc* siblingProcess;
  
  switch(cases){
	case 1:
		//Replace smallest virtual Runtime process with its right child 
		parentProcess = parentProc;
		childProcess = process->right;
		
		//if the process being removed is on the root
		if(process == tree->root){
			
			tree->root = childProcess;
			if(childProcess != 0){
				childProcess->parentP = 0;
				childProcess->coloring = BLACK;
			}
			
		} else if(childProcess != 0 && !(process->coloring == childProcess->coloring)){
			//Replace current process by it's right child
			childProcess->parentP = parentProcess;
			parentProcess->left = childProcess;
			childProcess->coloring = BLACK;		
		} else if(process->coloring == RED){		
			parentProcess->left = childProcess;
		} else {	
			if(childProcess != 0)
				childProcess->parentP = parentProcess;
			
			
			parentProcess->left = childProcess;
			retrievingCases(tree, parentProcess, childProcess, 2);
		}
		
		process->parentP = 0;
		process->left = 0;
		process->right = 0;
		parentProcess = 0;
		childProcess = 0;
		break;
		
	case 2:
		
		//Check if process is not root,i.e parentProc != 0, and process is black
		while(process != tree->root && (process == 0 || process->coloring == BLACK)){
			
			////Obtain sibling process
			if(process == parentProc->left){
				siblingProcess = parentProc->right;
				
				if(siblingProcess != 0 && siblingProcess->coloring == RED){
					siblingProcess->coloring = BLACK;
					parentProc->coloring = RED;
					rotateLeft(tree, parentProc);
					siblingProcess = parentProc->right;
				}
				if((siblingProcess->left == 0 || siblingProcess->left->coloring == BLACK) && (siblingProcess->right == 0 || siblingProcess->right->coloring == BLACK)){
					siblingProcess->coloring = RED;
					//Change process pointer and parentProc pointer
					process = parentProc;
					parentProc = parentProc->parentP;
				} else {
					if(siblingProcess->right == 0 || siblingProcess->right->coloring == BLACK){
						//Color left child
						if(siblingProcess->left != 0){
							siblingProcess->left->coloring = BLACK;
						} 
						siblingProcess->coloring = RED;
						rotateRight(tree, siblingProcess);
						siblingProcess = parentProc->right;
					}
					
					siblingProcess->coloring = parentProc->coloring;
					parentProc->coloring = BLACK;
					siblingProcess->right->coloring = BLACK;
					rotateLeft(tree, parentProc);
					process = tree->root;
				}
			} 
		}
		if(process != 0)
			process->coloring = BLACK;
		
		break;
	
	default:
		break;
  }
  return;
	
}

struct proc*
retrieveProcess(struct redblackTree* tree){
  struct proc* foundProcess;	//Process pointer utilized to hold the address of the process with smallest VRuntime 

  acquire(&tree->lock);
  if(!emptyTree(tree)){

	//If the number of processes are greater than the division between latency and minimum granularity
	//then recalculate the period for the processes
	//This condition is performed when the scheduler selects the next process to run
        //The formula can be found in CFS tuning article by Jacek Kobus and Refal Szklarski
	//In the CFS schduler tuning section:
	if(tree->count > (latency / min_granularity)){
		tree->period = tree->count * min_granularity;
	} 

	//retrive the process with the smallest virtual runtime by removing it from the red black tree and returning it
	foundProcess = tree->min_vRuntime;	

	//Determine if the process that is being chosen is runnable at the time of the selection, if it is not, then don't return it.
	if(foundProcess->state != RUNNABLE){
  		release(&tree->lock);
		return 0;
	}

	retrievingCases(tree, tree->min_vRuntime->parentP, tree->min_vRuntime, 1);
	tree->count -= 1;

	//Determine new process with the smallest virtual runtime
	tree->min_vRuntime = setMinimumVRuntimeproc(tree->root);

	//Calculate retrieved process's time slice based on formula: period*(process's weight/ red black tree weight)
	//Where period is the length of the epoch
	//The formula can be found in CFS tuning article by Jacek Kobus and Refal Szklarski
	//In the scheduling section:
	foundProcess->maximumExecutiontime = (tree->period * foundProcess->weightValue / tree->rbTreeWeight);
	
	//Recalculate total weight of red-black tree
	tree->rbTreeWeight -= foundProcess->weightValue;
  } else 
	foundProcess = 0;

  release(&tree->lock);
  return foundProcess;
}

////////

/*
  
  checkPreemption(struct proc*, struct proc*)
  parameters:the currently running/selected process and the process with the smallest vruntime in the red black tree
  return: an iteger value that dictates whether preemption should occur
  This function will determine if the process should be preempted.
  Preemption Cases:
  1-if the current running process virtual runtime is greater than the smallest virtual runtime
  2-if current running process currentRuntime has exceeded the maximum execution time
  3-Allow the current running process to continue running until preemption should occur

*/
int
checkPreemption(struct proc* current, struct proc* min_vruntime){

  //Utilize integer variable to compare current runtime with the minimum granularity
  int procRuntime = current->currentRuntime;
  
  //Determine if the currently running process has exceed its time slice.
  if((procRuntime >= current->maximumExecutiontime) && (procRuntime >= min_granularity)){
  	return 1;
  }

  //If the virtual runtime of the currently running process is greater than the smallest process, 
  //then context switching should occur
  if(min_vruntime != 0 && min_vruntime->state == RUNNABLE && current->virtualRuntime > min_vruntime->virtualRuntime){
	
	//Allow preemption if the process has ran for at least the min_granularity.
        //Due to the calls of checking for preemption, there needs to be made a distinction between when the preemption function
	//is called after a process has just be selected by the cfs scheduler and when a process has been currently running.
	if((procRuntime != 0) && (procRuntime >= min_granularity)){
		return 1;
  	} else if(procRuntime == 0){
		return 1;
        }
  }

  //No preemption should occur
  return 0;
}

void
pinit(void)
{
  initlock(&ptable.lock, "ptable");
  rbinit(runnableTasks, "runnableTasks");
}

//PAGEBREAK: 32
// Look in the process table for an UNUSED proc.
// If found, change state to EMBRYO and initialize
// state required to run in the kernel.
// Otherwise return 0.
static struct proc*
allocproc(void)
{
  struct proc *p;
  char *sp;

  acquire(&ptable.lock);
  for(p = ptable.proc; p < &ptable.proc[NPROC]; p++)
    if(p->state == UNUSED)
      goto found;
  release(&ptable.lock);
  return 0;

found:
  p->state = EMBRYO;
  p->pid = nextpid++;
  release(&ptable.lock);

  // Allocate kernel stack.
  if((p->kstack = kalloc()) == 0){
    p->state = UNUSED;
    return 0;
  }
  sp = p->kstack + KSTACKSIZE;
  
  // Leave room for trap frame.
  sp -= sizeof *p->tf;
  p->tf = (struct trapframe*)sp;
  
  // Set up new context to start executing at forkret,
  // which returns to trapret.
  sp -= 4;
  *(uint*)sp = (uint)trapret;

  sp -= sizeof *p->context;
  p->context = (struct context*)sp;
  memset(p->context, 0, sizeof *p->context);
  p->context->eip = (uint)forkret;

  //Set up fields of the process that are used by the CFS, which include fields to implement red-black tree
  p->virtualRuntime = 0;
  p->currentRuntime = 0;
  p->maximumExecutiontime = 0;
  p->niceValue = 0;

  p->left = 0;
  p->right = 0;
  p->parentP = 0;
  
  return p;
}

//PAGEBREAK: 32
// Set up first user process.
void
userinit(void)
{
  struct proc *p;
  extern char _binary_initcode_start[], _binary_initcode_size[];
  
  p = allocproc();
  initproc = p;
  if((p->pgdir = setupkvm()) == 0)
    panic("userinit: out of memory?");
  inituvm(p->pgdir, _binary_initcode_start, (int)_binary_initcode_size);
  p->sz = PGSIZE;
  memset(p->tf, 0, sizeof(*p->tf));
  p->tf->cs = (SEG_UCODE << 3) | DPL_USER;
  p->tf->ds = (SEG_UDATA << 3) | DPL_USER;
  p->tf->es = p->tf->ds;
  p->tf->ss = p->tf->ds;
  p->tf->eflags = FL_IF;
  p->tf->esp = PGSIZE;
  p->tf->eip = 0;  // beginning of initcode.S

  safestrcpy(p->name, "initcode", sizeof(p->name));
  p->cwd = namei("/");

  p->state = RUNNABLE;

  //Insert allocated process into the red black tree, which will become runnable, i.e its state will be set to runnable, after this function returns to the caller.
  insertProcess(runnableTasks, p);
}

// Grow current process's memory by n bytes.
// Return 0 on success, -1 on failure.
int
growproc(int n)
{
  uint sz;
  
  sz = proc->sz;
  if(n > 0){
    if((sz = allocuvm(proc->pgdir, sz, sz + n)) == 0)
      return -1;
  } else if(n < 0){
    if((sz = deallocuvm(proc->pgdir, sz, sz + n)) == 0)
      return -1;
  }
  proc->sz = sz;
  switchuvm(proc);
  return 0;
}

// Create a new process copying p as the parent.
// Sets up stack to return as if from system call.
// Caller must set state of returned proc to RUNNABLE.
int
fork(void)
{
  int i, pid;
  struct proc *np;

  // Allocate process.
  if((np = allocproc()) == 0)
    return -1;

  // Copy process state from p.
  if((np->pgdir = copyuvm(proc->pgdir, proc->sz)) == 0){
    kfree(np->kstack);
    np->kstack = 0;
    np->state = UNUSED;
    return -1;
  }
  np->sz = proc->sz;
  np->parent = proc;
  *np->tf = *proc->tf;

  // Clear %eax so that fork returns 0 in the child.
  np->tf->eax = 0;

  for(i = 0; i < NOFILE; i++)
    if(proc->ofile[i])
      np->ofile[i] = filedup(proc->ofile[i]);
  np->cwd = idup(proc->cwd);

  safestrcpy(np->name, proc->name, sizeof(proc->name));
 
  pid = np->pid;

  // lock to force the compiler to emit the np->state write last.
  acquire(&ptable.lock);
  np->state = RUNNABLE;
  release(&ptable.lock);

  //Insert allocated process into the red black tree, which will become runnable, i.e its state will be set to runnable, after this function returns to the caller.
  insertProcess(runnableTasks, np);
  
  return pid;
}

// Exit the current process.  Does not return.
// An exited process remains in the zombie state
// until its parent calls wait() to find out it exited.
void
exit(void)
{
  struct proc *p;
  int fd;

  if(proc == initproc)
    panic("init exiting");

  // Close all open files.
  for(fd = 0; fd < NOFILE; fd++){
    if(proc->ofile[fd]){
      fileclose(proc->ofile[fd]);
      proc->ofile[fd] = 0;
    }
  }

  begin_op();
  iput(proc->cwd);
  end_op();
  proc->cwd = 0;

  acquire(&ptable.lock);

  // Parent might be sleeping in wait().
  wakeup1(proc->parent);

  // Pass abandoned children to init.
  for(p = ptable.proc; p < &ptable.proc[NPROC]; p++){
    if(p->parent == proc){
      p->parent = initproc;
      if(p->state == ZOMBIE)
        wakeup1(initproc);
    }
  }

  // Jump into the scheduler, never to return.
  proc->state = ZOMBIE;
  sched();
  panic("zombie exit");
}

// Wait for a child process to exit and return its pid.
// Return -1 if this process has no children.
int
wait(void)
{
  struct proc *p;
  int havekids, pid;

  acquire(&ptable.lock);
  for(;;){
    // Scan through table looking for zombie children.
    havekids = 0;
    for(p = ptable.proc; p < &ptable.proc[NPROC]; p++){
      if(p->parent != proc)
        continue;
      havekids = 1;
      if(p->state == ZOMBIE){
        // Found one.
        pid = p->pid;
        kfree(p->kstack);
        p->kstack = 0;
        freevm(p->pgdir);
        p->state = UNUSED;
        p->pid = 0;
        p->parent = 0;
        p->name[0] = 0;
        p->killed = 0;
        release(&ptable.lock);
        return pid;
      }
    }

    // No point waiting if we don't have any children.
    if(!havekids || proc->killed){
      release(&ptable.lock);
      return -1;
    }

    // Wait for children to exit.  (See wakeup1 call in proc_exit.)
    sleep(proc, &ptable.lock);  //DOC: wait-sleep
  }
}

//PAGEBREAK: 42
// Per-CPU process scheduler.
// Each CPU calls scheduler() after setting itself up.
// Scheduler never returns.  It loops, doing:
//  - choose a process to run
//  - swtch to start running that process
//  - eventually that process transfers control
//      via swtch back to the scheduler.
void
scheduler(void)
{
  struct proc *p;
  
  for(;;){
    // Enable interrupts on this processor.
    sti();

    // Loop over process table looking for process to run.
    acquire(&ptable.lock);
    
    //Select process from red black tree, i.e readyTree
    p = retrieveProcess(runnableTasks);
    while(p != 0){

	if(p->state == RUNNABLE){
				
      		// Switch to chosen process.  It is the process's job
      		// to release ptable.lock and then reacquire it
      		// before jumping back to us.
      		proc = p;
      		switchuvm(p);
      		p->state = RUNNING;
      		swtch(&cpu->scheduler, proc->context);
      		switchkvm();
			
      		// Process is done running for now.
      		// It should have changed its p->state before coming back.
      		proc = 0;

	} 

	p = retrieveProcess(runnableTasks);
    }
    release(&ptable.lock);

  }
}

// Enter scheduler.  Must hold only ptable.lock
// and have changed proc->state.
void
sched(void)
{
  int intena;

  if(!holding(&ptable.lock))
    panic("sched ptable.lock");
  if(cpu->ncli != 1)
    panic("sched locks");
  if(proc->state == RUNNING)
    panic("sched running");
  if(readeflags()&FL_IF)
    panic("sched interruptible");
  intena = cpu->intena;
  swtch(&proc->context, cpu->scheduler);
  cpu->intena = intena;
}

//Determine if the currently running process should be preempted or allowed to continue running
void
yield(void)
{
  acquire(&ptable.lock);  //DOC: yieldlock

  //If preemption should occur, function returns 1
  //If it true then process's state will be set to runnable and its virtual time will be calculated
  if(checkPreemption(proc, runnableTasks->min_vRuntime) == 1){
  	proc->state = RUNNABLE;
	proc->virtualRuntime = proc->virtualRuntime + proc->currentRuntime;
        proc->currentRuntime = 0;
	insertProcess(runnableTasks, proc);
  	sched();
  }
  
  release(&ptable.lock);
}

// A fork child's very first scheduling by scheduler()
// will swtch here.  "Return" to user space.
void
forkret(void)
{
  static int first = 1;
  // Still holding ptable.lock from scheduler.
  release(&ptable.lock);

  if (first) {
    // Some initialization functions must be run in the context
    // of a regular process (e.g., they call sleep), and thus cannot 
    // be run from main().
    first = 0;
    initlog();
  }
  
  // Return to "caller", actually trapret (see allocproc).
}

// Atomically release lock and sleep on chan.
// Reacquires lock when awakened.
void
sleep(void *chan, struct spinlock *lk)
{
  if(proc == 0)
    panic("sleep");

  if(lk == 0)
    panic("sleep without lk");

  // Must acquire ptable.lock in order to
  // change p->state and then call sched.
  // Once we hold ptable.lock, we can be
  // guaranteed that we won't miss any wakeup
  // (wakeup runs with ptable.lock locked),
  // so it's okay to release lk.
  if(lk != &ptable.lock){  //DOC: sleeplock0
    acquire(&ptable.lock);  //DOC: sleeplock1
    release(lk);
  }

  // Go to sleep.
  proc->chan = chan;
  proc->state = SLEEPING;
  sched();

  // Tidy up.
  proc->chan = 0;

  // Reacquire original lock.
  if(lk != &ptable.lock){  //DOC: sleeplock2
    release(&ptable.lock);
    acquire(lk);
  }
}

//PAGEBREAK!
// Wake up all processes sleeping on chan.
// The ptable lock must be held.
static void
wakeup1(void *chan)
{
  struct proc *p;

  for(p = ptable.proc; p < &ptable.proc[NPROC]; p++)
    if(p->state == SLEEPING && p->chan == chan){
      p->state = RUNNABLE;

      //Update runtime stats of process being woken up
      p->virtualRuntime = p->virtualRuntime + p->currentRuntime;
      p->currentRuntime = 0;

      //Insert process after it has finished Sleeping
      insertProcess(runnableTasks, p);
    }
}

// Wake up all processes sleeping on chan.
void
wakeup(void *chan)
{
  acquire(&ptable.lock);
  wakeup1(chan);
  release(&ptable.lock);
}

// Kill the process with the given pid.
// Process won't exit until it returns
// to user space (see trap in trap.c).
int
kill(int pid)
{
  struct proc *p;

  acquire(&ptable.lock);
  for(p = ptable.proc; p < &ptable.proc[NPROC]; p++){
    if(p->pid == pid){
      p->killed = 1;
      // Wake process from sleep if necessary.
      if(p->state == SLEEPING){
        p->state = RUNNABLE;

        //Update runtime stats of process being killed
        p->virtualRuntime = p->virtualRuntime + p->currentRuntime;
        p->currentRuntime = 0;

        //insert process into runnableTask tree
        insertProcess(runnableTasks, p);
      }
      release(&ptable.lock);
      return 0;
    }
  }
  release(&ptable.lock);
  return -1;
}

//PAGEBREAK: 36
// Print a process listing to console.  For debugging.
// Runs when user types ^P on console.
// No lock to avoid wedging a stuck machine further.
void
procdump(void)
{
  static char *states[] = {
  [UNUSED]    "unused",
  [EMBRYO]    "embryo",
  [SLEEPING]  "sleep ",
  [RUNNABLE]  "runnable",
  [RUNNING]   "run   ",
  [ZOMBIE]    "zombie"
  };
  int i;
  struct proc *p;
  char *state;
  uint pc[10];
  
  for(p = ptable.proc; p < &ptable.proc[NPROC]; p++){
    if(p->state == UNUSED)
      continue;
    if(p->state >= 0 && p->state < NELEM(states) && states[p->state])
      state = states[p->state];
    else
      state = "???";
    cprintf("%d %s %s", p->pid, state, p->name);
    if(p->state == SLEEPING){
      getcallerpcs((uint*)p->context->ebp+2, pc);
      for(i=0; i<10 && pc[i] != 0; i++)
        cprintf(" %p", pc[i]);
    }
    cprintf("\n");
  }
}
