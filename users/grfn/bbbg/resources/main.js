window.onload = () => {
  console.log("loaded");
  const input = document.getElementById("name-autocomplete");
  if (input != null) {
    const eventID = document.getElementById("event-id").value;

    const autocomplete = new autoComplete({
      selector: "#name-autocomplete",
      placeHolder: "Enter your name",
      data: {
        src: async (query) => {
          const resp = await fetch(
            `/attendees.json?q=${query}&event_id=${eventID}&attended=false`
          );
          console.log("got resp");
          const { results } = await resp.json();
          return results;
        },
        keys: ["bbbg.attendee/meetup-name"],
      },
      resultItem: {
        highlight: {
          render: true,
        },
      },
    });

    input.addEventListener("selection", function (event) {
      const attendee = event.detail.selection.value;
      event.target.value = attendee["bbbg.attendee/meetup-name"];

      const attendeeID = attendee["bbbg.attendee/id"];
      document.getElementById("attendee-id").value = attendeeID;
      document.getElementById("signup-form").removeAttribute("disabled");
      document
        .getElementById("signup-form")
        .querySelector('input[type="submit"]')
        .removeAttribute("disabled");
    });
  }

  document.querySelectorAll("form").forEach((form) => {
    form.onsubmit = (e) => {
      if (e.target.attributes.disabled) {
        e.preventDefault();
      }
    };
  });
};
