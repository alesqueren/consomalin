/* global $ */

$('#newGroup').on('submit', function submit(e) {
  e.preventDefault();
  $.ajax({
    type: $(this).attr('method'),
    url: $(this).attr('action'),
    data: $(this).serialize(),
    complete: function complete(responseObject) {
      $('#response').html(responseObject.responseText);
    },
  });
});
