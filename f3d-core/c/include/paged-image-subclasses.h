// This is included from paged-image-accessors.C

#define IREF_SETUP(ELEMENT_TYPE) \
  MAP_ELEMENT_TYPE index = iref_index(x, y);\
  page_map *page_map = page_handler;\
  int blknum = index >> TILE_OFFSET_BITS(this);	\
  int offset = index & TILE_OFFSET_MASK(this);		\
  page_queue_entry *entry = page_map->array[blknum];\
  ELEMENT_TYPE *block; 


// Move to paged-image-accessors.C to avoid multiple vtables in IRIX CC
// This requires all templetes for paged_image to be in paged-image-accessors.C
template <class eltype>
class paged_image: public paged_image_base
{public:
  static eltype example_element;
  IMAGE_ELEMENT_TYPE element_type();
  paged_image(int sx, int sy, IMAGE_ELEMENT_TYPE el_type, int samples_per_pixel,
	      int bx, int by, int pbx, int no_alloc, int offset_bits=0);
  paged_image();
  inline int _iref(int x,int y) const 
   {
     IREF_SETUP(eltype);
     if (((entry->status) & PAGE_READ_OK_BIT) == 0)
       entry = page_handler->read_page_fault(blknum);
     block = (eltype *) ((image_page_queue_entry *)entry)->page;
     return((int) block[offset]);
    }  
  inline double _diref(int x,int y) const 
    {
      IREF_SETUP(eltype);
      if (((entry->status) & PAGE_READ_OK_BIT) == 0)
	entry = page_handler->read_page_fault(blknum);
      block = (eltype *) ((image_page_queue_entry *)entry)->page;
      return((int) block[offset]);
    }  
  inline void _iset(int x,int y, int val)
    {
      IREF_SETUP(eltype);
      if (((entry->status) & PAGE_WRITE_OK_BIT) == 0)
	entry = page_handler->write_page_fault(blknum);
      block = (eltype *) ((image_page_queue_entry *)entry)->page;
      block[offset] = (eltype) val;
    }  
  inline void _iset(int x,int y, double val)
    {
      IREF_SETUP(eltype);
      if (((entry->status) & PAGE_WRITE_OK_BIT) == 0)
	entry = page_handler->write_page_fault(blknum);
      block = (eltype *) ((image_page_queue_entry *)entry)->page;
      block[offset] = (eltype) val;
    }  
  inline double _interpolate_iref (double x, double y)
    {
      int ix = (int) x; int iy = (int) y;
      double wx = x - (double) ix; 
      double wy = y - (double) iy;
      double v00 = _diref (ix, iy)   , v10 = _diref (ix+1, iy);
      double v01 = _diref (ix, iy+1) , v11 = _diref (ix+1, iy+1);
      double interp0 = v00 + wx*(v10 - v00);
      double interp1 = v01 + wx*(v11 - v01);
      return (interp0 + wy*(interp1 - interp0));
    }

  int iref(int x, int y);
  double interpolate_iref(double x, double y);
  double diref(int x, int y);
  void iset(int x, int y, int val);
  void iset(int x, int y, double val);

  virtual int getline (int    *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (double *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (float *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (int    *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putline (double *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putline (float *buf, int x, int y, int n=0, int to_index=0, int band=0);

  virtual int getcolumn (int    *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getcolumn (double *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getcolumn (float *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putcolumn (int    *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putcolumn (double *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putcolumn (float *buf, int x, int y, int n=0, int to_index=0, int band=0);

//  virtual int getline (unsigned char *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
//  virtual int putline (unsigned char *buf, int x, int y, int n=0, int to_index=0, int band=0);
  
#if 0
  virtual int getline (unsigned char *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (unsigned short *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int getline (float *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (unsigned char *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putline (unsigned short *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int putline (float *buf, int x, int y, int n=0, int to_index=0, int band=0);
#endif

};



template <class eltype>
class band_interleaved_paged_image: public paged_image_base
{public:
  static eltype example_element;
  IMAGE_ELEMENT_TYPE element_type();
  band_interleaved_paged_image(int sx, int sy, IMAGE_ELEMENT_TYPE elemtype, int samples_per_pixel,
		     int bx, int by, int pbx, int no_alloc, int offset_bits=0);
  band_interleaved_paged_image();
  inline void _viref(int x,int y, eltype val) const 
   {
     IREF_SETUP(eltype);
     if (((entry->status) & PAGE_READ_OK_BIT) == 0)
       entry = page_handler->read_page_fault(blknum);
     block = (eltype *) ((image_page_queue_entry *)entry)->page;
     val = block[offset];
    }  
  inline void _iset(int x,int y, eltype val)
    {
      IREF_SETUP(eltype);
      if (((entry->status) & PAGE_WRITE_OK_BIT) == 0)
	entry = page_handler->write_page_fault(blknum);
      block = (eltype *) ((image_page_queue_entry *)entry)->page;
      block[offset] = (eltype) val;
    }  
  void viref(int x, int y, eltype val);
  void iset(int x, int y, eltype val);

  virtual int getline (eltype    *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putline (eltype    *buf, int x, int y, int n=0, int to_index=0, int band=0);
  virtual int getline (void *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0)
    {return getline((eltype *)buf, x, y, n, to_index, dx, band);}
  virtual int putline (void *buf, int x, int y, int n=0, int to_index=0, int band=0)
    {return putline((eltype *)buf, x, y, n, to_index, band);} 
  virtual int getcolumn (eltype    *buf, int x, int y, int n=0, int to_index=0, int dx=1, int band=0);
  virtual int putcolumn (eltype    *buf, int x, int y, int n=0, int to_index=0, int band=0);
};


