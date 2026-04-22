import React from "react";
import { createRoot } from "react-dom/client";
import { Agentation } from "agentation";

const rootNode = document.getElementById("agentation-root");

if (rootNode) {
  const endpoint = rootNode.dataset.endpoint || undefined;
  const sessionId = rootNode.dataset.sessionId || undefined;
  const root = createRoot(rootNode);

  root.render(
    React.createElement(Agentation, {
      endpoint,
      sessionId,
      copyToClipboard: true,
    }),
  );
}
