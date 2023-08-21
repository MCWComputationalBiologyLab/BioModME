function showPlot(base64Img, width, height) {
    var newWindow = window.open("", "", "width=" + width + ",height=" + height);
    newWindow.document.write("<img src=\'" + base64Img + "\' alt=\'Plot\'>");
    newWindow.document.close();
 }