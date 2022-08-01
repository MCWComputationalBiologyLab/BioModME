$(document).keyup(function(event) {
    if ($("#createVar_varInput").is(":focus") && (event.key == "Enter")) {
        $("#createVar_addVarToList").click();
    }
});