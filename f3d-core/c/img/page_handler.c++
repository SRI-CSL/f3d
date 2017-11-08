/* CC -g -c -I ~/cp/include  page_handler.C
gcc -c -I ~/cp/include  page_handler.C
*/

#include "misc.h"
#include "cme-error.h"
#include "page_handler.h"
#include "cme_time.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

int pager_debug = 0;


page_queue_entry::page_queue_entry()
{
  queue = 0;
  next = 0;
  prev = 0;
  status = 0;
  owner = 0;
  owning_map_index = 0;  
}

void
page_queue_entry::free_page_data()
{}

page_queue::page_queue ()
{
  head = new page_queue_entry();
  head->next=head->prev=head;
  head->queue=this;
  entry_count = 0;
}

page_queue_entry *
add_entry_to_page_queue_tail (page_queue_entry *entry, page_queue *queue)
{
  page_queue_entry *head = queue->head;
  page_queue_entry *prev = head->prev;
  if (head) 
    {entry->queue = head->queue;
    head->queue->entry_count++;
    }

  entry->prev = prev;
  prev->next = entry;
  entry->next = head;
  head->prev = entry;
  return entry;
}

page_queue_entry *
add_entry_to_page_queue_head (page_queue_entry *entry, page_queue *queue) 
{
  tprintf(7, "add_entry_to_page_queue_head: entry=0x%x, queue=0x%x\n", entry, queue);
  if (entry == 0) {
    tprintf(7, "add_entry_to_page_queue_head: zero entry?\n");
    return entry;
  }

  page_queue_entry *head = queue->head;
  page_queue_entry *next = head->next;
  tprintf(7, "add_entry_to_page_queue_head: head=0x%x, next=0x%x, entry_count = %d\n",
	  head, next, head->queue->entry_count);
  if (head) {
    entry->queue = head->queue;
    head->queue->entry_count++;
  }

 tprintf(7, "add_entry_to_page_queue_head: head->queue->entry_count = %d.\n",
	 head->queue->entry_count);

 entry->next = next;
 next->prev = entry;
 entry->prev = head;
 head->next = entry;
 tprintf(7, "add_entry_to_page_queue_head: done.\n");

 return entry;
}

page_queue_entry *
remove_entry_from_page_queue(page_queue_entry *entry, page_queue *queue)
{
  if (! queue) {
    queue = entry->queue;
    if (! queue) error("remove_entry_from_page_queue: queue_entry doesn't belong to any queue.");
  }
 
  queue->entry_count--;
  {
    page_queue_entry *next = entry->next;
    page_queue_entry *prev = entry->prev;
    next->prev = prev;
    prev->next = next;
    entry->prev = 0;
    entry->next = 0;
    entry->queue = 0;
  }
  return entry;
}

page_queue_entry *
remove_entry_from_page_queue_head(page_queue* queue)
{
  page_queue_entry *head = queue->head;
  page_queue_entry *entry;
  if (head->next != head) {
    entry= remove_entry_from_page_queue(head->next, queue);
    return entry;
  } else return 0;
}

page_queue_entry *
remove_entry_from_page_queue_tail(page_queue* queue)
{
  page_queue_entry *head = queue->head;
  if (head->prev != head) {
    return (remove_entry_from_page_queue( head->prev, queue));
  } else return 0;
}

// function is not allowed to change the structure of the queue. ???
void map_over_page_queue(page_queue *queue, void (*function)(page_queue_entry *entry))
{
  int iters;
  page_queue_entry *start = queue->head;
  page_queue_entry *entry = start->next;
  page_queue_entry *next;
  for (iters=0; entry!=start & iters<10000; iters++)
    {
      next = entry->next; // this allows function to delete entry.
      (*function)(entry);
      entry = next;
    }
}


//POINTER_LIST page_pool_list;

page_queue* make_page_queue()
{ 
  return new page_queue();
}


void
page_pool::free_page_data (page_queue_entry *entry)
{
  entry->free_page_data();
}

page_queue_entry *
page_pool::new_page_queue_entry()
{
  return 0;
}

page_pool::page_pool(int pool_size_pages):Object()
{
  initial_pool_size = pool_size_pages;
  pool_size = 0;
  /* standard defaults */
  free_page_queue   = make_page_queue();
  busy_page_queue   = make_page_queue();
  aged_page_queue   = make_page_queue();
  locked_page_queue = make_page_queue();
  percent_aged      = .5;
  pool_growth_rate  =.25;
  read_count=0;
  write_count=0;
  miss_count=0;
  probe_count=0;
  working_set_count = 5;
  //  page_map_list     = (POINTER_LIST) NIL;// Nothing uses page_map_list
  zero_time(&page_handling_run_time);
  zero_time(&page_handling_elapsed_time);
  rejuvenate_count = 0;
  // resize_page_pool (pool_size_pages);
}

int Page_pool_resize_verbose = FALSE;

void
page_pool::resize_page_pool (int page_change_count)
{
  int i, n;
  if (pool_size + page_change_count < 1)
    page_change_count = 1 - pool_size;
  if (Page_pool_resize_verbose) {
    warning("Resizing page-pool %p to contain %d pages\n", this, pool_size + page_change_count);
  }
    
  n = abs( page_change_count);
  tprintf(4, "page_pool::resize_page_pool: abs(page_change_count) = %d\n", n);
  for (i=0; i<n; i++) {
    tprintf(7, "page_pool::resize_page_pool: adding new page queue entry %d\n", i);
    if (page_change_count > 0)
      add_entry_to_page_queue_head (this->new_page_queue_entry(),
				    free_page_queue);
    else {
      page_queue_entry *entry = find_free_page_entry(this);
      tprintf(4, "free page entry: 0x%x\n", entry);
      if (entry) {
	remove_entry_from_page_queue(entry, free_page_queue);
	//	  entry->free_page_data();
	free_page_data(entry);
      }
    }
  }
  pool_size += page_change_count;
  tprintf(5, "page_pool::resize_page_pool : pool_size is now %d.\n", pool_size);
}

int
page_queue_length (page_queue *queue)
{
  int n = 0;
  int iters;
  page_queue_entry *start = queue->head;
  page_queue_entry *entry = start->next;
  page_queue_entry *next;
  for (iters=0; entry!=start & iters<10000; iters++)
    {
      next = entry->next; // this allows function to delete entry.
      n++;
      entry = next;
    }
  return n;
}
  
void
set_page_pool_size (page_pool *page_pool, int pool_size)
{
  page_pool->resize_page_pool(pool_size - page_pool->pool_size);
}

void
page_pool_stats (page_pool *page_pool, int resetp)
{
  double hit_ratio = (page_pool->probe_count - page_pool->miss_count) 
    / ((float) MAX(1, page_pool->probe_count));

  warning("Page Pool of %d pages: %d busy, %d aged, %d free, page-hit-ratio=%f probes =%d misses=%d reads=%d writes=%d run_time=%f elapsed_time=%f\n",
	  page_pool->pool_size,
	  page_queue_length(page_pool->busy_page_queue),
	  page_queue_length(page_pool->aged_page_queue),
	  page_queue_length(page_pool->free_page_queue),
	  hit_ratio, 
	  page_pool->probe_count,
	  page_pool->miss_count,
	  page_pool->read_count, 
	  page_pool->write_count,
	  time_secs(&page_pool->page_handling_run_time),
	  time_secs(&page_pool->page_handling_elapsed_time)
	  );
 
  if (resetp)
    {page_pool->miss_count = page_pool->probe_count 
       = page_pool->read_count = page_pool->write_count = 0;
    zero_time(&page_pool->page_handling_run_time );
    zero_time(&page_pool->page_handling_elapsed_time );
    }
}

void
age_page_queue_entry (page_queue_entry* entry)
{
  entry->status 
    = (entry->status & ~(PAGE_READ_OK_BIT | PAGE_WRITE_OK_BIT )) | PAGE_AGED_BIT;
  entry->owner->set_page_status(entry->owning_map_index, entry->status);
  if (pager_debug) 
    warning("Aging page %d, status=%d\n", 
	    entry->owning_map_index, entry->status);
}


void 
add_page_map(page_pool* page_pool, page_map* page_map)
{
  // Nothing uses page_map_list
  //page_pool->page_map_list = cons((char *) page_map, page_pool->page_map_list);
}


page_map::page_map():Object()
{
  page_pool = 0;
  map_size = 0;
  array = 0;
  write_permit = 0;
  active_tile_count = 0;
  page_initialization_map = 0;
}

void
page_map::set_write_permit (int write_permit)
{
  if (write_permit) {
    if (!page_initialization_map)
      page_initialization_map = ZALLOC(char, map_size);
    this->write_permit = 1;
  } else {
    if (page_initialization_map) {
      fprintf(stderr, "page_map::set_write_permit(0): %p\n", page_initialization_map);
      xxfree(page_initialization_map);
    }
    page_initialization_map = 0;
    this->write_permit = 0;
  }
}

page_map::page_map(int size, Page_Handler *ph, Page_Pool* pool, int wrt_permit )
  :Object()
{
  page_pool = pool; 
  map_size = size;
  array = XALLOC(page_queue_entry *, map_size);
  tprintf(4, "page_map::page_map: created page_map %p pool=%p size=%d, null_page=%p\n", 
	  ph, page_pool, map_size, page_pool->null_page);
  for (int i=0; i<map_size; i++) 
    array[i]=page_pool->null_page;
  active_tile_count = 0;
  page_initialization_map = 0;
  ph->set_write_permit (wrt_permit);
  add_page_map(page_pool, this);
  tprintf(4, "page_map::page_map: returning.\n");
} 

void
age_page (page_map* page_map, int page_number)
{
  age_page_queue_entry(page_map_entry(page_map, page_number));
}

void give_up_page(basic_page_handler* pm, int page_number);

void 
validate_queue(page_queue *queue, const char* name, const char* context, page_map *map )
{
  int iters;
  page_queue_entry *start = queue->head;
  page_queue_entry *entry = start->next;
  page_queue_entry *next;
  for (iters=0; entry!=start & iters<10000; iters++)
    {
      next = entry->next; // this allows function to delete entry.
      if (! entry->owner)
	warning("%s %s queue has ownerless entry \n", context, name);
      else if (map && entry->owner != map)
	warning("%s %s entry points to wrong map\n", context, name);
       
      if (entry->queue != queue)
	warning("%s %s entry points to wrong queue\n", context, name);
     
      entry = next;
    }
  if (iters != queue->entry_count)
    warning("%s %s queue entry count %d is wrong, should be %d\n", 
	    context, name, queue->entry_count, iters);
}
 
void
validate_queues(page_pool* pool, const char* context, page_map *map=0)
{
  if (0) {
    validate_queue(pool->busy_page_queue, "busy", context, map);
    validate_queue(pool->aged_page_queue, "aged", context, map);
  }
}


void
balance_queues (page_pool* pool)
{
  int busy_count = pool->busy_page_queue->entry_count;
  int free_count = pool->free_page_queue->entry_count; 
  int aged_count = pool->aged_page_queue->entry_count;
  int aged_plus_free_count = free_count + aged_count;
  int total_count = busy_count + aged_plus_free_count;
  int desired_aged = (int)  (pool->percent_aged * total_count);
  int deficiency = desired_aged - aged_plus_free_count;
  validate_queues(pool, "balance_queues1");
  if (pager_debug) 
    warning("balance_queues: %d %d %d deficiency=%d\n",
	    busy_count, free_count, aged_count, deficiency);

  for (; deficiency>0; deficiency--) {
    page_queue_entry* entry;
    entry = remove_entry_from_page_queue_tail(pool->busy_page_queue);
    if (! entry) return;  // should this be break instead of return ?
    if (entry->owner) {
      add_entry_to_page_queue_head (entry, pool->aged_page_queue);
      age_page(entry->owner, entry->owning_map_index);
    }
    else {
      add_entry_to_page_queue_tail(entry, pool->free_page_queue);
      warning("balance_queues found ownerless page in pool\n");
    }
  }
  validate_queues(pool, "balance_queues2");
}

#if 0 // this is a broken version Mon Apr 19 2004
void
free_page_queue_entry (page_queue_entry* entry)
{
  if (entry->owner) {
    give_up_page(entry->owner, entry->owning_map_index);
    entry->owner = NIL;
  }
 entry->status = 0;
 if (entry->owner)
   entry->owner->set_page_status(entry->owning_map_index, 0);
}
#endif

void
free_page_queue_entry (page_queue_entry* entry)
{
  if (entry->owner) {
    give_up_page(entry->owner, entry->owning_map_index);
    entry->owner->set_page_status(entry->owning_map_index, 0);
    entry->owner = NIL;
  }
 entry->status = 0;
}

void
free_page_pool_entry (page_pool* pool, page_queue_entry* entry)
{
  remove_entry_from_page_queue(entry, entry->queue);
  add_entry_to_page_queue_head(entry, pool->free_page_queue);
  free_page_queue_entry(entry);
}

void
free_page (page_map* page_map, int page_number)
{
  page_queue_entry* entry =  page_map_entry(page_map, page_number);
  if (entry != page_map->page_pool->null_page)
    free_page_pool_entry (page_map->page_pool, entry);
}

void
free_page_map (page_map* page_map)
{
  int i;
  validate_queues(page_map->page_pool, "free_page_map1", page_map);
  page_queue_entry* null_page = page_map->page_pool->null_page;
  for (i=0; i < page_map->map_size; i++)
    if (page_map_entry(page_map, i) != null_page)
      free_page(page_map, i); 
  validate_queues(page_map->page_pool, "free_page_map2", page_map);

}

void
flush_map(page_map* page_map)
{
  free_page_map(page_map);
}

page_queue_entry*
possibly_create_new_entry (page_pool* page_pool)
{
// This is called only when the pool was never initially resized to initial_pool_size
  if (page_pool->pool_size < page_pool->initial_pool_size) {
    page_pool->resize_page_pool(page_pool->initial_pool_size - page_pool->pool_size);
    return(remove_entry_from_page_queue_head(page_pool->free_page_queue));
  }
  return(0);
}

// This version implements LRU using an aging queue.  
// This is sometimes called the "Clock Algorithm".  
page_queue_entry*
find_free_page_entry (page_pool* page_pool)
{
  page_queue_entry* entry;
  entry = remove_entry_from_page_queue_head(page_pool->free_page_queue);
  if (! entry) entry = remove_entry_from_page_queue_tail(page_pool->aged_page_queue);
  if (! entry) entry = remove_entry_from_page_queue_tail(page_pool->busy_page_queue);
  if (! entry) entry = possibly_create_new_entry(page_pool);
  if (! entry) error ("find_free_page_entry:  Shouldn't happen.");
  balance_queues(page_pool);
  add_entry_to_page_queue_head(entry, page_pool->busy_page_queue);
  free_page_queue_entry(entry);
  return(entry);
}

void
setup_page(page_map* page_map, int page_number, page_queue_entry* entry)
{
  page_map->active_tile_count++;
  page_map_entry(page_map, page_number) = entry;
} 


page_queue_entry*
assign_page(page_pool* page_pool, page_queue_entry* entry, Page_Handler *owner, int owning_map_index)
{
  entry->owner = owner;
  entry->owning_map_index = owning_map_index;
  setup_page(owner, owning_map_index, entry);
  return(entry);
} 



page_queue_entry*
rejuvenate_page(page_map* page_map,  page_queue_entry* entry)
{
  remove_entry_from_page_queue(entry, entry->queue);
  if (! entry->owner) warning("found ownerless page in rejuvenate_page\n");
  add_entry_to_page_queue_head(entry, page_map->page_pool->busy_page_queue);
  page_map->page_pool->rejuvenate_count++;
  if (entry->status & PAGE_DIRTY_BIT) {
    if (page_map->write_permit)
      entry->status = PAGE_READ_OK_BIT  | PAGE_WRITE_OK_BIT | PAGE_DIRTY_BIT;
    else
      entry->status = PAGE_READ_OK_BIT | PAGE_DIRTY_BIT;
  }
  else entry->status = PAGE_READ_OK_BIT;
  entry->owner->set_page_status(entry->owning_map_index, entry->status);
  if (pager_debug) warning("rejuvenate_page %d\n", entry->owning_map_index);
  return(entry);
}

void
lock_page_pool_page(page_pool* page_pool, page_queue_entry* entry)
{
  if (entry) {
    remove_entry_from_page_queue(entry, entry->queue);
    add_entry_to_page_queue_head(entry, page_pool->locked_page_queue);}
}

void 
unlock_page_pool_page(page_pool* page_pool, page_queue_entry* entry)
{
  if (entry) {
    remove_entry_from_page_queue(entry, entry->queue);
    add_entry_to_page_queue_head(entry, page_pool->busy_page_queue);}
}
 
void
unlock_page(page_map* page_map, int page_number)
{
  unlock_page_pool_page(page_map->page_pool, page_map_entry(page_map, page_number));
}


void 
basic_page_handler::set_page_status (int page_number, int status)
{}

#if 0
int
basic_page_handler::save_image (char *path)
{
  return 0;
}
#endif

extern void 
give_up_page(basic_page_handler* ph, int page_number)
{
  ph->write_page(page_number);
  page_map_entry(ph, page_number) = ph->page_pool->null_page;
  ph->active_tile_count--;
  if (pager_debug) warning("give_up_page %d\n", page_number);
}

void basic_page_handler::tile_builder(int x0, int y0, int x1, int y1, char *page)
{
  error("basic_page_handler::tile_builder is not defined.\n");
}


basic_page_handler::basic_page_handler()
{
  //  img = 0;
}

basic_page_handler::basic_page_handler(Page_Pool * page_pool, int npages, int write_permit)  
  :page_map(npages, this, page_pool, write_permit) 
{
  // img = 0;
}


void
basic_page_handler::read_page (int blknum)
{
  error ("basic_page_handler::read_page undefined");
}

void
basic_page_handler::write_page (int blknum)
{
  error ("basic_page_handler::write_page undefined");
}

page_queue_entry *
basic_page_handler::read_page_fault (int blknum)
{
  error ("basic_page_handler::read_page_fault undefined");
  return(0);
}

page_queue_entry *
basic_page_handler::write_page_fault (int blknum)
{
  error ("basic_page_handler::write_page_fault undefined");
  return(0);
}

page_queue_entry *
basic_page_handler::probe_page_fault (int blknum)
{
  error ("basic_page_handler::probe_page_fault undefined");
  return(0);
}


void write_page(basic_page_handler *handler, int page_number)
{
  handler->write_page(page_number);
}

// Much of this is really specific to image page handling
page_queue_entry *
basic_page_handler::get_page (int page_number)
{
  Page_Pool *pool = page_pool;
  page_queue_entry *entry = page_map_entry(this, page_number);

  if (entry != pool->null_page)
    return(entry);
  entry = find_free_page_entry(pool);
  if (! entry) 
    error("basic_page_handler::get_page - find_free_page_entry returned NIL\n");

  assign_page(page_pool, entry, this, page_number);
  page_pool->miss_count++;

  tprintf(12, "basic_page_handler::get_page(%d) about to read page\n", page_number);
  /* This should be inside an UNWIND_PROTECT */
  {
    Page_Pool * previous_page_pool_tmp; /* Must be stack allocated */
    static Page_Pool * previous_page_pool = NIL; 
    Timeval user_start_time, sys_start_time, end_time, user_ticks, sys_ticks;
    run_time(&user_start_time); 
    elapsed_time(&sys_start_time);
    previous_page_pool_tmp = previous_page_pool; /* Simulate Lisp let-binding */
    previous_page_pool = pool;
    read_page(page_number);
    time_diff(run_time(&end_time), &user_start_time, &user_ticks);
    time_add(&page_pool->page_handling_run_time, &user_ticks, NIL);
    time_diff(elapsed_time(&end_time), &sys_start_time, &sys_ticks);
    time_add(&page_pool->page_handling_elapsed_time, &sys_ticks, NIL);
    
    if (previous_page_pool_tmp) {
      time_diff(&previous_page_pool_tmp->page_handling_run_time, &user_ticks, NIL);
      time_diff(&previous_page_pool_tmp->page_handling_elapsed_time, &sys_ticks, NIL);
    }
    previous_page_pool = previous_page_pool_tmp; // restore 
  }
  return(entry);
}

END_NAMESPACE_FREEDIUS
