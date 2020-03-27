import React from "react";

function ProgressBar(props: {
  done: number;
  total: number;
  units: string;
  color: string;
}) {
  const { done, total, units, color } = props;
  const width = Math.floor((done / total) * 100);
  const rest = 100 - width;

  let [fg, bg] = [`bg-${color}-600`, `bg-${color}-100`];

  if (color === "white") {
    [fg, bg] = ["bg-gray-600", "bg-gray-100"];
  }

  return (
    <div className={`relative ${bg} h-5`}>
      <div
        className={`${fg} h-5 absolute top-0 left-0`}
        style={{ width: `${width}%` }}
      ></div>
      <p className="absolute text-xs pl-1 pt-1">
        {done} of {total} {units}
      </p>
    </div>
  );
}

function Goal(props: {
  subject: string;
  goal: string;
  done: number;
  total: number;
  units: string;
  color: string;
}) {
  const { subject, goal, done, total, units, color } = props;
  const width = "6em";

  const Tr = (props: {
    label: string;
    value: string;
    valueComponent?: React.ReactElement;
  }) => (
    <tr className="flex py-2">
      <td className="text-gray-600" style={{ width: width }}>
        {props.label}
      </td>
      <td className="flex-1">
        {props.valueComponent ? props.valueComponent : props.value}
      </td>
    </tr>
  );

  return (
    <table className="w-full mb-10">
      <tbody>
        <Tr label="Subject" value={subject} />
        <Tr label="Goal" value={goal} />
        <Tr
          label="Progress"
          value={goal}
          valueComponent={
            <ProgressBar
              done={done}
              total={total}
              units={units}
              color={color}
            />
          }
        />
      </tbody>
    </table>
  );
}

function Copy(props: { children: React.ReactNode }) {
  return <p className="pb-4 leading-loose">{props.children}</p>;
}

function App() {
  return (
    <div className="container mx-auto font-mono">
      <section>
        <h1 className="text-center pt-12 pb-6">Goals</h1>
        <Copy>
          For me, a goal is something that is difficult for me to complete but
          easy for me to measure. I tend to add new goals as time progresses,
          mistakenly assuming that I can support additional goals for free. To
          counterbalance my tendancy to casually accumulate goals, I aim to only
          have three goals; I will not add a new goal until I complete an
          existing goal. I created and published this page to clarify that idea.
        </Copy>
        <Copy>
          Here are my current goals and the progress I have made towards
          achieving them.
        </Copy>
      </section>
      <section className="pt-4">
        <Goal
          subject="Meditation"
          goal="Meditate for 10,000 hours"
          done={100}
          total={10000}
          units="hrs"
          color="purple"
        />
        <Goal
          subject="Debt"
          goal="Pay my student debt balance"
          done={30000}
          total={70000}
          units="USD"
          color="green"
        />
        <Goal
          subject="Brazilian Jiu Jitsu"
          goal="Train until an instructor gives me a black belt"
          done={1}
          total={5}
          units="belts"
          color="white"
        />
      </section>
    </div>
  );
}

export default App;
