/* Generated by wbuild
 * (generator version 3.2)
 */
#ifndef ___XWGROUPP_H
#define ___XWGROUPP_H
#include <./xwRowColP.h>
#include <./xwGroup.h>
#include <stdint.h>
_XFUNCPROTOBEGIN

typedef struct {
/* methods */
/* class variables */
int dummy;
} XfwfGroupClassPart;

typedef struct _XfwfGroupClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfRowColClassPart xfwfRowCol_class;
XfwfGroupClassPart xfwfGroup_class;
} XfwfGroupClassRec;

typedef struct {
/* resources */
String  label;
XFontStruct * font;
Pixel  foreground;
SelectionType  selectionStyle;
long  selection;
XtCallbackList  activate;
/* private state */
GC  textgc;
uintptr_t  toggle_ord;
} XfwfGroupPart;

typedef struct _XfwfGroupRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfRowColPart xfwfRowCol;
XfwfGroupPart xfwfGroup;
} XfwfGroupRec;

externalref XfwfGroupClassRec xfwfGroupClassRec;

_XFUNCPROTOEND
#endif /* ___XWGROUPP_H */
