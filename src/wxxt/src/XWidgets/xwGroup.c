/* Generated by wbuild
 * (generator version 3.2)
 */
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include <stdint.h>
#include <X11/Xmu/Converters.h>
#include <X11/Xmu/CharSet.h>
#include <xwToggle.h>
#include <./xwGroupP.h>
static void _resolve_inheritance(
#if NeedFunctionPrototypes
WidgetClass
#endif
);
static void class_initialize(
#if NeedFunctionPrototypes
void
#endif
);
static void initialize(
#if NeedFunctionPrototypes
Widget ,Widget,ArgList ,Cardinal *
#endif
);
static void destroy(
#if NeedFunctionPrototypes
Widget
#endif
);
static Boolean  set_values(
#if NeedFunctionPrototypes
Widget ,Widget ,Widget,ArgList ,Cardinal *
#endif
);
static void _expose(
#if NeedFunctionPrototypes
Widget,XEvent *,Region 
#endif
);
static void insert_child(
#if NeedFunctionPrototypes
Widget 
#endif
);
static void make_textgc(
#if NeedFunctionPrototypes
Widget
#endif
);
static void on_cb(
#if NeedFunctionPrototypes
Widget ,XtPointer ,XtPointer 
#endif
);
static void off_cb(
#if NeedFunctionPrototypes
Widget ,XtPointer ,XtPointer 
#endif
);
static void set_toggles(
#if NeedFunctionPrototypes
Widget
#endif
);
#define done(type, value) do {\
      if (to->addr != NULL) {\
	  if (to->size < sizeof(type)) {\
	      to->size = sizeof(type);\
	      return False;\
	  }\
	  *(type*)(to->addr) = (value);\
      } else {\
	  static type static_val;\
	  static_val = (value);\
	  to->addr = (XtPointer)&static_val;\
      }\
      to->size = sizeof(type);\
      return True;\
  }while (0 )


static Boolean  cvtStringToSelectionType(
#if NeedFunctionPrototypes
Display *,XrmValuePtr ,Cardinal *,XrmValuePtr ,XrmValuePtr ,XtPointer *
#endif
);
static Boolean  cvtSelectionTypeToString(
#if NeedFunctionPrototypes
Display *,XrmValuePtr ,Cardinal *,XrmValuePtr ,XrmValuePtr ,XtPointer *
#endif
);
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void make_textgc(Widget self)
#else
static void make_textgc(self)Widget self;
#endif
{
    XtGCMask mask;
    XGCValues values;

    if (((XfwfGroupWidget)self)->xfwfGroup.textgc != NULL) XtReleaseGC(self, ((XfwfGroupWidget)self)->xfwfGroup.textgc);
    values.background = ((XfwfGroupWidget)self)->core.background_pixel;
    values.foreground = ((XfwfGroupWidget)self)->xfwfGroup.foreground;
    mask = GCBackground | GCForeground;
    if (((XfwfGroupWidget)self)->xfwfGroup.font) {
      values.font = ((XfwfGroupWidget)self)->xfwfGroup.font->fid;
      mask |= GCFont;
    }
    ((XfwfGroupWidget)self)->xfwfGroup.textgc = XtGetGC(self, mask, &values);
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void on_cb(Widget  toggle,XtPointer  client_data,XtPointer  call_data)
#else
static void on_cb(toggle,client_data,call_data)Widget  toggle;XtPointer  client_data;XtPointer  call_data;
#endif
{
    Widget self = XtParent(toggle);
    uintptr_t toggle_ord = (uintptr_t) client_data;
    Cardinal t, i, bits = sizeof(((XfwfGroupWidget)self)->xfwfGroup.selection) * 8;

    switch (((XfwfGroupWidget)self)->xfwfGroup.selectionStyle) {
    case XfwfMultipleSelection:
	if (toggle_ord < bits) ((XfwfGroupWidget)self)->xfwfGroup.selection |= 1L << toggle_ord;
	break;
    case XfwfSingleSelection:
    case XfwfOneSelection:
	if (((XfwfGroupWidget)self)->xfwfGroup.selection != -1L)
	    for (t = 0, i = 0; i < ((XfwfGroupWidget)self)->composite.num_children; i++)
		if (XtIsSubclass(((XfwfGroupWidget)self)->composite.children[i], xfwfToggleWidgetClass)) {
		    if (((XfwfGroupWidget)self)->xfwfGroup.selection == t) {
			XtVaSetValues(((XfwfGroupWidget)self)->composite.children[i], XtNon, False, NULL);
			break;
		    }
		    t++;
		}
	((XfwfGroupWidget)self)->xfwfGroup.selection = toggle_ord;
	break;
    default: ;
    }
    XtCallCallbackList(self, ((XfwfGroupWidget)self)->xfwfGroup.activate, (XtPointer) ((XfwfGroupWidget)self)->xfwfGroup.selection);
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void off_cb(Widget  toggle,XtPointer  client_data,XtPointer  call_data)
#else
static void off_cb(toggle,client_data,call_data)Widget  toggle;XtPointer  client_data;XtPointer  call_data;
#endif
{
    Widget self = XtParent(toggle);
    uintptr_t toggle_ord = (uintptr_t) client_data;
    Cardinal bits = sizeof(((XfwfGroupWidget)self)->xfwfGroup.selection) * 8;

    switch (((XfwfGroupWidget)self)->xfwfGroup.selectionStyle) {
    case XfwfOneSelection:
	XtVaSetValues(toggle, XtNon, True, NULL); /* Undo */
	break;
    case XfwfSingleSelection:
	((XfwfGroupWidget)self)->xfwfGroup.selection = -1L;			/* Nothing selected */
	break;
    case XfwfMultipleSelection:
	if (toggle_ord < bits) ((XfwfGroupWidget)self)->xfwfGroup.selection &= ~(1L << toggle_ord);
	break;
    default: ;
    }
    XtCallCallbackList(self, ((XfwfGroupWidget)self)->xfwfGroup.activate, (XtPointer) ((XfwfGroupWidget)self)->xfwfGroup.selection);
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void set_toggles(Widget self)
#else
static void set_toggles(self)Widget self;
#endif
{
    Cardinal i, t;

    for (t = 0, i = 0; i < ((XfwfGroupWidget)self)->composite.num_children; i++)
	if (XtIsSubclass(((XfwfGroupWidget)self)->composite.children[i], xfwfToggleWidgetClass)) {
	    switch (((XfwfGroupWidget)self)->xfwfGroup.selectionStyle) {
	    case XfwfNoSelection:
		break;
	    case XfwfSingleSelection:
	    case XfwfOneSelection:
		XtVaSetValues(((XfwfGroupWidget)self)->composite.children[i], XtNon, t == ((XfwfGroupWidget)self)->xfwfGroup.selection, NULL);
		break;
	    case XfwfMultipleSelection:
		XtVaSetValues(((XfwfGroupWidget)self)->composite.children[i],
			      XtNon, (((XfwfGroupWidget)self)->xfwfGroup.selection & (1L<<t)) != 0, NULL);
		break;
	    }
	    t++;
	}
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static Boolean  cvtStringToSelectionType(Display * display,XrmValuePtr  args,Cardinal * num_args,XrmValuePtr  from,XrmValuePtr  to,XtPointer * converter_data)
#else
static Boolean  cvtStringToSelectionType(display,args,num_args,from,to,converter_data)Display * display;XrmValuePtr  args;Cardinal * num_args;XrmValuePtr  from;XrmValuePtr  to;XtPointer * converter_data;
#endif
{
    String s = (String) from->addr;

    if (*num_args != 0)
	XtAppErrorMsg(XtDisplayToApplicationContext(display),
		      "cvtStringToSelectionType", "wrongParameters",
		      "XtToolkitError",
		      "String to SelectionType conversion needs no arguments",
		      (String*) NULL, (Cardinal*) NULL);

    if (XmuCompareISOLatin1(s, "no") == 0)
	done(SelectionType, XfwfNoSelection);
    if (XmuCompareISOLatin1(s, "none") == 0)
	done(SelectionType, XfwfNoSelection);
    if (XmuCompareISOLatin1(s, "single") == 0)
	done(SelectionType, XfwfSingleSelection);
    if (XmuCompareISOLatin1(s, "one") == 0)
	done(SelectionType, XfwfOneSelection);
    if (XmuCompareISOLatin1(s, "multi") == 0)
	done(SelectionType, XfwfMultipleSelection);
    if (XmuCompareISOLatin1(s, "multiple") == 0)
	done(SelectionType, XfwfMultipleSelection);

    XtDisplayStringConversionWarning(display, s, XtRSelectionType);
    done(SelectionType, XfwfSingleSelection);
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static Boolean  cvtSelectionTypeToString(Display * display,XrmValuePtr  args,Cardinal * num_args,XrmValuePtr  from,XrmValuePtr  to,XtPointer * converter_data)
#else
static Boolean  cvtSelectionTypeToString(display,args,num_args,from,to,converter_data)Display * display;XrmValuePtr  args;Cardinal * num_args;XrmValuePtr  from;XrmValuePtr  to;XtPointer * converter_data;
#endif
{
    char s[30];

    if (*num_args != 0)
	XtAppErrorMsg(XtDisplayToApplicationContext(display),
		      "cvtStringToSelectionStyle", "wrongParameters",
		      "XtToolkitError",
		      "String to SelectionStyle conversion needs no arguments",
		      (String*) NULL, (Cardinal*) NULL);
    switch (*((SelectionType*) from->addr)) {
    case XfwfNoSelection: done(String, "none");
    case XfwfSingleSelection: done(String, "single");
    case XfwfOneSelection: done(String, "one");
    case XfwfMultipleSelection: done(String, "multiple");
    }
    XtDisplayStringConversionWarning(display, s, XtRSelectionType);
    done(String, "none");
}

static XtResource resources[] = {
{XtNlabel,XtCLabel,XtRString,sizeof(((XfwfGroupRec*)NULL)->xfwfGroup.label),XtOffsetOf(XfwfGroupRec,xfwfGroup.label),XtRImmediate,(XtPointer)NULL },
{XtNfont,XtCFont,XtRFontStruct,sizeof(((XfwfGroupRec*)NULL)->xfwfGroup.font),XtOffsetOf(XfwfGroupRec,xfwfGroup.font),XtRString,(XtPointer)XtDefaultFont },
{XtNforeground,XtCForeground,XtRPixel,sizeof(((XfwfGroupRec*)NULL)->xfwfGroup.foreground),XtOffsetOf(XfwfGroupRec,xfwfGroup.foreground),XtRString,(XtPointer)XtDefaultForeground },
{XtNselectionStyle,XtCSelectionStyle,XtRSelectionType,sizeof(((XfwfGroupRec*)NULL)->xfwfGroup.selectionStyle),XtOffsetOf(XfwfGroupRec,xfwfGroup.selectionStyle),XtRImmediate,(XtPointer)XfwfSingleSelection },
{XtNselection,XtCSelection,XtRLong,sizeof(((XfwfGroupRec*)NULL)->xfwfGroup.selection),XtOffsetOf(XfwfGroupRec,xfwfGroup.selection),XtRImmediate,(XtPointer)0 },
{XtNactivate,XtCActivate,XtRCallback,sizeof(((XfwfGroupRec*)NULL)->xfwfGroup.activate),XtOffsetOf(XfwfGroupRec,xfwfGroup.activate),XtRImmediate,(XtPointer)NULL },
{XtNframeType,XtCFrameType,XtRFrameType,sizeof(((XfwfGroupRec*)NULL)->xfwfFrame.frameType),XtOffsetOf(XfwfGroupRec,xfwfFrame.frameType),XtRImmediate,(XtPointer)XfwfChiseled },
{XtNinnerOffset,XtCInnerOffset,XtRDimension,sizeof(((XfwfGroupRec*)NULL)->xfwfFrame.innerOffset),XtOffsetOf(XfwfGroupRec,xfwfFrame.innerOffset),XtRImmediate,(XtPointer)0 },
};

XfwfGroupClassRec xfwfGroupClassRec = {
{ /* core_class part */
/* superclass   	*/  (WidgetClass) &xfwfRowColClassRec,
/* class_name   	*/  "XfwfGroup",
/* widget_size  	*/  sizeof(XfwfGroupRec),
/* class_initialize 	*/  class_initialize,
/* class_part_initialize*/  _resolve_inheritance,
/* class_inited 	*/  FALSE,
/* initialize   	*/  initialize,
/* initialize_hook 	*/  NULL,
/* realize      	*/  XtInheritRealize,
/* actions      	*/  NULL,
/* num_actions  	*/  0,
/* resources    	*/  resources,
/* num_resources 	*/  8,
/* xrm_class    	*/  NULLQUARK,
/* compres_motion 	*/  True ,
/* compress_exposure 	*/  XtExposeCompressMaximal ,
/* compress_enterleave 	*/  True ,
/* visible_interest 	*/  False ,
/* destroy      	*/  destroy,
/* resize       	*/  XtInheritResize,
/* expose       	*/  XtInheritExpose,
/* set_values   	*/  set_values,
/* set_values_hook 	*/  NULL,
/* set_values_almost 	*/  XtInheritSetValuesAlmost,
/* get_values+hook 	*/  NULL,
/* accept_focus 	*/  XtInheritAcceptFocus,
/* version      	*/  XtVersion,
/* callback_private 	*/  NULL,
/* tm_table      	*/  NULL,
/* query_geometry 	*/  XtInheritQueryGeometry,
/* display_acceleator 	*/  XtInheritDisplayAccelerator,
/* extension    	*/  NULL 
},
{ /* composite_class part */
XtInheritGeometryManager,
XtInheritChangeManaged,
insert_child,
XtInheritDeleteChild,
NULL
},
{ /* XfwfCommon_class part */
XtInherit_compute_inside,
XtInherit_total_frame_width,
_expose,
XtInherit_highlight_border,
XtInherit_unhighlight_border,
XtInherit_hilite_callbacks,
XtInherit_would_accept_focus,
XtInherit_traverse,
XtInherit_lighter_color,
XtInherit_darker_color,
XtInherit_set_color,
/* traversal_trans */  NULL ,
/* traversal_trans_small */  NULL ,
/* travMode */  1 ,
},
{ /* XfwfFrame_class part */
 /* dummy */  0
},
{ /* XfwfBoard_class part */
XtInherit_set_abs_location,
},
{ /* XfwfRowCol_class part */
XtInherit_layout,
},
{ /* XfwfGroup_class part */
 /* dummy */  0
},
};
WidgetClass xfwfGroupWidgetClass = (WidgetClass) &xfwfGroupClassRec;
static void _resolve_inheritance(class)
WidgetClass class;
{
  XfwfGroupWidgetClass c = (XfwfGroupWidgetClass) class;
  XfwfGroupWidgetClass super;
  static CompositeClassExtensionRec extension_rec = {
    NULL, NULLQUARK, XtCompositeExtensionVersion,
    sizeof(CompositeClassExtensionRec), True};
  CompositeClassExtensionRec *ext;
  ext = (XtPointer)XtMalloc(sizeof(*ext));
  *ext = extension_rec;
  ext->next_extension = c->composite_class.extension;
  c->composite_class.extension = ext;
  if (class == xfwfGroupWidgetClass) return;
  super = (XfwfGroupWidgetClass)class->core_class.superclass;
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void class_initialize(void)
#else
static void class_initialize()
#endif
{
    XtAddConverter(XtRString, XtRLong, XmuCvtStringToLong, NULL, 0);
    XtSetTypeConverter(XtRString, XtRSelectionType, cvtStringToSelectionType,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRSelectionType, XtRString, cvtSelectionTypeToString,
		       NULL, 0, XtCacheNone, NULL);
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void initialize(Widget  request,Widget self,ArgList  args,Cardinal * num_args)
#else
static void initialize(request,self,args,num_args)Widget  request;Widget self;ArgList  args;Cardinal * num_args;
#endif
{
    ((XfwfGroupWidget)self)->xfwfGroup.toggle_ord = 0;
    ((XfwfGroupWidget)self)->xfwfGroup.textgc = NULL;
    make_textgc(self);
    if (((XfwfGroupWidget)self)->xfwfGroup.label)
     ((XfwfGroupWidget)self)->xfwfGroup.label = XtNewString(((XfwfGroupWidget)self)->xfwfGroup.label);
    if (((XfwfGroupWidget)self)->xfwfGroup.selectionStyle == XfwfOneSelection && ((XfwfGroupWidget)self)->xfwfGroup.selection == -1L) {
	XtWarning
	    ("Illegal combination of selectionStyle and selection resources");
	((XfwfGroupWidget)self)->xfwfGroup.selection = 0;
    }
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void destroy(Widget self)
#else
static void destroy(self)Widget self;
#endif
{
   if (((XfwfGroupWidget)self)->xfwfGroup.textgc) XtReleaseGC(self, ((XfwfGroupWidget)self)->xfwfGroup.textgc); ((XfwfGroupWidget)self)->xfwfGroup.textgc = NULL;
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static Boolean  set_values(Widget  old,Widget  request,Widget self,ArgList  args,Cardinal * num_args)
#else
static Boolean  set_values(old,request,self,args,num_args)Widget  old;Widget  request;Widget self;ArgList  args;Cardinal * num_args;
#endif
{
    Boolean need_redraw = False;

    if (((XfwfGroupWidget)old)->xfwfGroup.label != ((XfwfGroupWidget)self)->xfwfGroup.label) {
	XtFree(((XfwfGroupWidget)old)->xfwfGroup.label);
	((XfwfGroupWidget)self)->xfwfGroup.label = XtNewString(((XfwfGroupWidget)self)->xfwfGroup.label);
	need_redraw = True;
    }
    if (((XfwfGroupWidget)self)->xfwfGroup.font != ((XfwfGroupWidget)old)->xfwfGroup.font) {
	make_textgc(self);
	if (((XfwfGroupWidget)self)->xfwfGroup.label != NULL) need_redraw = True;
    }
    if (((XfwfGroupWidget)old)->xfwfGroup.selection != ((XfwfGroupWidget)self)->xfwfGroup.selection
	|| ((XfwfGroupWidget)old)->xfwfGroup.selectionStyle != ((XfwfGroupWidget)self)->xfwfGroup.selectionStyle) {
	if (((XfwfGroupWidget)self)->xfwfGroup.selectionStyle == XfwfOneSelection && ((XfwfGroupWidget)self)->xfwfGroup.selection == -1L)
	    ((XfwfGroupWidget)self)->xfwfGroup.selection = 0;
	set_toggles(self);
    }

    return need_redraw;
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void _expose(Widget self,XEvent * event,Region  region)
#else
static void _expose(self,event,region)Widget self;XEvent * event;Region  region;
#endif
{
    int w, h;
    Position x, y;

    if (! XtIsRealized(self)) return;
    xfwfRowColClassRec.xfwfCommon_class._expose(self, event, region);
    if (((XfwfGroupWidget)self)->xfwfGroup.label) {
	((XfwfGroupWidgetClass)self->core.widget_class)->xfwfCommon_class.compute_inside(self, &x, &y, &w, &h);

	XDrawImageString(XtDisplay(self), XtWindow(self), ((XfwfGroupWidget)self)->xfwfGroup.textgc, x + 3,
			 y - ((XfwfGroupWidget)self)->xfwfFrame.innerOffset, ((XfwfGroupWidget)self)->xfwfGroup.label, strlen(((XfwfGroupWidget)self)->xfwfGroup.label));
    }

   
}
/*ARGSUSED*/
#if NeedFunctionPrototypes
static void insert_child(Widget  child)
#else
static void insert_child(child)Widget  child;
#endif
{ Widget self = XtParent(child); {
    xfwfRowColClassRec.composite_class.insert_child(child);
    if (((XfwfGroupWidget)self)->xfwfGroup.selectionStyle != XfwfNoSelection
	&& XtIsSubclass(child, xfwfToggleWidgetClass)) {
	XtAddCallback(child, XtNonCallback, on_cb, (XtPointer) ((XfwfGroupWidget)self)->xfwfGroup.toggle_ord);
	XtAddCallback(child, XtNoffCallback, off_cb, (XtPointer) ((XfwfGroupWidget)self)->xfwfGroup.toggle_ord);
	switch (((XfwfGroupWidget)self)->xfwfGroup.selectionStyle) {
	case XfwfOneSelection:
	case XfwfSingleSelection:
	    XtVaSetValues(child, XtNon, ((XfwfGroupWidget)self)->xfwfGroup.toggle_ord == ((XfwfGroupWidget)self)->xfwfGroup.selection,
	    			 XtNindicatorType, XfwfDiamondIndicator, NULL);
	    break;
	case XfwfMultipleSelection:
	    XtVaSetValues(child, XtNon, (((XfwfGroupWidget)self)->xfwfGroup.selection & (1L<<((XfwfGroupWidget)self)->xfwfGroup.toggle_ord)) != 0,
	    			 XtNindicatorType, XfwfSquareIndicator, NULL);
	    break;
	default: ;
	}
	((XfwfGroupWidget)self)->xfwfGroup.toggle_ord++;
    }
}
}
