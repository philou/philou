// ==========================================================================
// Project:   SproutCore - JavaScript Application Framework
// Copyright: ©2006-2009 Sprout Systems, Inc. and contributors.
//            Portions ©2008-2009 Apple Inc. All rights reserved.
// License:   Licened under MIT license (see license.js)
// ==========================================================================
/*globals TestRunner */

/**
  Displayed when the app has no targets.
*/
TestRunner.NO_TARGETS = SC.Responder.create({
  
  /**
    Show laoding targets view.
  */
  didBecomeFirstResponder: function() {
    TestRunner.set('currentScene', 'noTargets');
    TestRunner.updateRoute(null, null, YES);
  },
  
  willLoseFirstResponder: function() {
    TestRunner.set('currentScene', null);
  }
    
});; if ((typeof SC !== 'undefined') && SC && SC.scriptDidLoad) SC.scriptDidLoad('sproutcore/tests');