#ifndef	__page_handler_h
#define __page_handler_h

#include "cme_time.h"
#include "image-types.h" 

#include "base-class.h"

#include "namespace.h"

BEGIN_NAMESPACE_FREEDIUS

// LHQ Sat Jul 23 2005: LISP_ARRAY_HEADER_OFFSET should be a global computed at run-time.
#define RUNTIME_LISP_ARRAY_HEADER_OFFSET

#if !defined(RUNTIME_LISP_ARRAY_HEADER_OFFSET)

/*
These #defines should move to a header file(s) for Lisp dependent constants

The page slot of an image_page_queue_entry contains the address of the first element of a Lisp
allocated array.  Adding LISP_ARRAY_HEADER_OFFSET to this address creates the proper Lisp tag for
the array.

The function TEST-LISP-ARRAY-REPN in the  file $FREEDIUS/lisp/img/paged-image.lisp
performs a runtime check that LISP_ARRAY_HEADER_OFFSET is correctly defined.
*/

//#define LISP_ARRAY_HEADER_OFFSET 0
#ifdef HAVE_ALLEGRO
// This has changed in Allegro bummer
//#define LISP_ARRAY_HEADER_OFFSET -2

// Platform-dependent offset:
#ifdef DARWIN
#define LISP_ARRAY_HEADER_OFFSET -10
#else
// Allegro 6.0:
#define LISP_ARRAY_HEADER_OFFSET 14
// Allegro 7.0 on Windows (Linux too?)
//#define LISP_ARRAY_HEADER_OFFSET 10
#endif

#endif // HAVE_ALLEGRO

#ifdef HAVE_LUCID
#define LISP_ARRAY_HEADER_OFFSET 2
#endif // HAVE_LUCID

#if defined(HAVE_CMUCL)  || defined(HAVE_SBCL)
#define LISP_ARRAY_HEADER_OFFSET -1
#endif // defined(HAVE_CMUCL)  || defined(HAVE_SBCL)


#else // defined(RUNTIME_LISP_ARRAY_HEADER_OFFSET)

extern int LISP_ARRAY_HEADER_OFFSET;
#endif // defined(RUNTIME_LISP_ARRAY_HEADER_OFFSET)


//typedef char Page_Handler;
typedef class basic_page_handler Page_Handler;
//void write_page(Page_Handler *handler, int page_number);

#define page_map_entry(page_map, page_number) page_map->array[page_number]
#define PAGE_READ_OK_BIT 1
#define PAGE_WRITE_OK_BIT 2
#define PAGE_DIRTY_BIT 4
#define PAGE_AGED_BIT 8

   // forward decls
typedef class page_queue Page_Queue;
typedef class page_queue_entry Page_Queue_Entry;
typedef class page_map Page_Map;
typedef class page_pool Page_Pool;

// PAGE_QUEUE_ENTRY  ********************************************

// PAGE_QUEUE_ENTRY has been simplified to eliminate all virtual functions
// so that it is really a "plain-old C struct".

class page_queue_entry // :public Object 
{
 public:
  Page_Queue *queue;
  page_queue_entry *next;
  page_queue_entry *prev;
  int status;
  Page_Handler *owner;			// pointer to page_map 
  int owning_map_index;
  virtual void free_page_data();
  //void free_page_data();
  page_queue_entry();
};

// PAGE_QUEUE ********************************************
class page_queue // :public Object 
{
 public:
  page_queue_entry *head;
  int entry_count;
  page_queue();
};

page_queue_entry *
add_entry_to_page_queue_head(page_queue_entry *entry, page_queue *queue);
page_queue_entry *
add_entry_to_page_queue_tail(page_queue_entry *entry, page_queue *queue);

page_queue_entry *
remove_entry_from_page_queue(page_queue_entry *entry, page_queue *queue);
page_queue_entry *remove_entry_from_page_queue_head(page_queue* queue);
page_queue_entry *remove_entry_from_page_queue_tail(page_queue* queue);

// PAGE_POOL  ********************************************
class page_pool :public Object {
public:
  page_queue *free_page_queue;
  page_queue *busy_page_queue;
  page_queue *aged_page_queue;
  page_queue *locked_page_queue;
  page_queue_entry *null_page;
  float percent_aged;
  int pool_size;
  int initial_pool_size;
  float pool_growth_rate;
  //  Pointer_List *page_map_list;
  int miss_count;
  int probe_count;
  int read_count;
  int write_count;
  int working_set_count;
  Timeval page_handling_run_time;
  Timeval page_handling_elapsed_time;
  int rejuvenate_count;
  page_pool(int pool_size_pages);
  virtual void resize_page_pool(int page_change_count);
  virtual page_queue_entry *new_page_queue_entry();  
  virtual void free_page_data (page_queue_entry *entry);  
};

page_queue_entry *find_free_page_entry(page_pool *pool);
void set_page_pool_size (page_pool *page_pool, int pool_size);



// **************************  PAGE_MAP  ************************************
class page_map :public Object {
public:
  Page_Pool *page_pool;
  int map_size;
  page_queue_entry **array; // pointer to array of pointers to page queue entries
  int write_permit;// ???
  int active_tile_count;
  char *page_initialization_map; // ???  
  //  Page_Handler *page_handler;       // not sure about this 
  page_map(int size, Page_Handler *ph, Page_Pool* pool, int wrt_permit = 0);
  page_map();  
  virtual void set_write_permit(int write_permit);
};

// Forward decl
class paged_image_base;

// ************************  BASIC_PAGE_HANDLER *********************************
class basic_page_handler :public page_map 
{public:
  // paged_image_base *img; // This looks wrong.  Added for lazy-image and file-image. Should move to image_page_handler
  basic_page_handler();
  basic_page_handler(Page_Pool * page_pool, int npages, int write_permit = 0);
  page_queue_entry * get_page(int page_number);  
  virtual page_queue_entry * read_page_fault (int blknum);
  virtual page_queue_entry * write_page_fault (int blknum);
  virtual page_queue_entry * probe_page_fault (int blknum);
  virtual void read_page(int blknum);
  virtual void write_page(int blknum);
  //  virtual off_t page_filepos(int page_number);
  // this this next needed?
  virtual void tile_builder(int x0, int y0, int x1, int y1, char *page);
  virtual void set_page_status (int page_number, int status);
  //  virtual int save_image (char *path);
};


void free_page_queue_entry (page_queue_entry* entry);
void free_page_pool_entry (page_pool* pool, page_queue_entry* entry);

void map_over_page_queue(page_queue *queue, 
			 void (*function)(page_queue_entry *entry));

page_pool* make_page_pool (int pool_size_pages, int bytes_per_page);

int get_page_pool_size (int bytes_per_page);

page_pool* get_page_pool (int bytes_per_page, int pool_size=-1);

page_queue_entry* rejuvenate_page(page_map* page_map,  page_queue_entry* entry);

page_queue_entry* get_page_entry (page_map* page_map, int page_number);

page_queue_entry* find_free_page_entry(page_pool* page_pool);

page_queue_entry* assign_page(page_pool* page_pool, page_queue_entry* entry, 
			      Page_Handler *owner, int owning_map_index);

void flush_map(page_map* page_map);
void free_page_map (page_map* page_map);
void page_pool_stats (page_pool *page_pool, int resetp);


END_NAMESPACE_FREEDIUS

#endif /* ! __page_handler_h */


