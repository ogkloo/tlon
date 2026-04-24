import React from 'react';
import { createRoot } from 'react-dom/client';
import { Agentation } from 'agentation';

const rootNode = document.getElementById('agentation-root');

if (rootNode) {
  try {
    sessionStorage.removeItem('agentation-session-toolbar-hidden');
  } catch (error) {
    console.warn('[tlon] Could not clear Agentation hidden state.', error);
  }

  const endpoint = rootNode.dataset.endpoint || undefined;
  const sessionId = rootNode.dataset.sessionId || undefined;

  createRoot(rootNode).render(
    <Agentation endpoint={endpoint} sessionId={sessionId} />
  );
}
