$( "#newGroup" ).on('submit', function(e) {
    e.preventDefault();
    $.ajax({
        type: $(this).attr('method'),
        url : $(this).attr('action'),
        data: $(this).serialize(),
        complete: function(responseObject) {
            $('#response').html(responseObject.responseText);
        }
    });
});