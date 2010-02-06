// ==========================================================================
// Project:   Todos.taskController
// Copyright: ©2009 My Company, Inc.
// ==========================================================================
/*globals Todos */

/** @class

  (Document Your Controller Here)

  @extends SC.ArrayController
*/
Todos.tasksController = SC.ArrayController.create(
  SC.CollectionViewDelegate,
/** @scope Todos.taskController.prototype */ {

  collectionViewDeleteContent: function(view, content, indexes) {
    // destroy the records
    var records = indexes.map(function(idx) {
      return this.objectAt(idx);
    }, this);
    records.invoke('destroy');

    var selIndex = indexes.get('min')-1;
    if (selIndex<0) selIndex = 0;
    this.selectObject(this.objectAt(selIndex));
  },

  addTask: function() {

    var task = Todos.store.createRecord(Todos.Task, {
      description: 'New task',
      isDone: NO
    });

    this.selectObject(task);

    this.invokeLater(function() {
      var contentIndex = this.indexOf(task);
      var list = Todos.mainPage.getPath('mainPane.middleView.contentView');
      var listItem = list.itemViewForContentIndex(contentIndex);
      listItem.beginEditing();
    });

    return YES;
  },

  toggleDone: function() {
    var sel = this.get('selection');
    sel.setEach('isDone', !sel.everyProperty('isDone'));
    return YES;
  },

  summary: function() {
    var len = this.get('length'), ret ;

    if (len && len > 0) {
      ret = len === 1 ? "1 task" : "%@ tasks".fmt(len);
    } else ret = "No tasks";

    return ret;
  }.property('length').cacheable()

}) ;
