window.onload = () => {
  const input = document.getElementById("name-autocomplete");
  if (input != null) {
    const attendeeList = document.getElementById("attendees-list");
    const filterAttendees = (filter) => {
      if (filter == "") {
        for (let elt of attendeeList.querySelectorAll("li")) {
          elt.classList.remove("hidden");
        }

        return;
      }

      let re = "";
      for (let c of filter) {
        re += `${c}.*`;
      }
      let filterRe = new RegExp(re, "i");

      for (let elt of attendeeList.querySelectorAll("li")) {
        const attendee = JSON.parse(elt.dataset.attendee);
        if (attendee["bbbg.attendee/meetup-name"].match(filterRe) == null) {
          elt.classList.add("hidden");
        } else {
          elt.classList.remove("hidden");
        }
      }
    };

    const attendeeIDInput = document.getElementById("attendee-id");
    const submit = document.querySelector("#submit-button");
    const signupForm = document.getElementById("signup-form");

    input.oninput = (e) => {
      filterAttendees(e.target.value);
      attendeeIDInput.value = null;
      submit.classList.add("hidden");
      submit.setAttribute("disabled", "disabled");
      signupForm.setAttribute("disabled", "disabled");
    };

    attendeeList.addEventListener("click", (e) => {
      if (!(e.target instanceof HTMLLIElement)) {
        return;
      }
      if (e.target.dataset.attendee == null) {
        return;
      }

      const attendee = JSON.parse(e.target.dataset.attendee);
      input.value = attendee["bbbg.attendee/meetup-name"];
      attendeeIDInput.value = attendee["bbbg.attendee/id"];

      submit.classList.remove("hidden");
      submit.removeAttribute("disabled");
      signupForm.removeAttribute("disabled");
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
