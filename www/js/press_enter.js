$(document).keyup(function(event) {
    if ($("#createVar_varInput").is(":focus") && (event.key == "Enter")) {
        $("#createVar_addVarToList").click();
    } else if ($("#createVar_compartment_input").is(":focus") && (event.key == "Enter")) {
        $("#createVar_add_compartment").click();
    }
});

