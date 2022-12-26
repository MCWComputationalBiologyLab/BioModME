Selectize.prototype.selectall = function(){
    var self = this;
    self.setValue(Object.keys(self.options));
  }