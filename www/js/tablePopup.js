function tablePopup(tableHTML, width, height) {
    var newWindow = window.open("", "", "width=" + width + ",height=" + height);
    newWindow.document.write(tableHTML);
    newWindow.document.close();
}