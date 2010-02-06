// ==========================================================================
// Project:   CoreTools
// Copyright: ©2009 Apple Inc.
// ==========================================================================
/*globals CoreTools */

/** @namespace

  This framework contains common code shared by the SproutCore developer tools
  including the test runner, doc viewer and welcome apps.  It is not generally
  intended for use in your own applications.
  
  @extends SC.Object
*/
CoreTools = SC.Object.create( /** @scope CoreTools.prototype */ {

  NAMESPACE: 'CoreTools',
  VERSION: '1.0.0'
  
}) ;
; if ((typeof SC !== 'undefined') && SC && SC.scriptDidLoad) SC.scriptDidLoad('sproutcore/core_tools');