/*
 * File:	wb_check.h
 * Purpose:	Check boxes
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_checkh
#define wxb_checkh

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

// Checkbox item (single checkbox)
class wxBitmap ;
class wxbCheckBox: public wxItem
{
 public:
  wxbCheckBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");
  wxbCheckBox(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");
  ~wxbCheckBox(void);

  virtual void SetValue(Bool) = 0;
  virtual Bool GetValue(void) = 0;
  // Avoids compiler warning
  inline void SetLabel(char *label) { wxItem::SetLabel(label) ; }
  virtual void SetLabel(wxBitmap *bitmap) = 0;
};

#endif // wxb_checkh
