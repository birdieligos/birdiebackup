## client.html 
<!DOCTYPE html>
  <html lang="en">
    <head>
    <meta charset="UTF-8" />
      <title>Task Checklist</title>
      <script src="https://p.trellocdn.com/power-up.min.js"></script>
        <style>
        body { font-family: sans-serif; padding: 10px; }
      .task-list { list-style: none; padding: 0; }
      .task-item {
        display: flex; align-items: center;
        background: #f4f5f7; padding: 8px; border-radius: 6px;
          margin-bottom: 6px; gap: 8px;
      }
      .task-name {
        flex: 1; font-weight: bold; font-size: 14px;
        border: 1px solid #ccc; border-radius: 4px; padding: 6px;
      }
      .set-date, .assign-member, .three-dots {
        background: #e2e4e6; border: none;
          padding: 6px 8px; border-radius: 4px;
        font-size: 12px; cursor: pointer;
        display: flex; align-items: center; gap: 4px;
      }
      .three-dots { font-size: 18px; padding: 5px 8px; }
      .add-task-btn {
        margin-top: 10px; padding: 10px;
        background-color: #0079bf; color: white;
          font-weight: bold; border: none;
        border-radius: 6px; width: 100%;
        cursor: pointer;
      }
      .task-input-row {
        display: flex; align-items: center; gap: 8px;
        margin-bottom: 10px;
      }
      .add-btn {
        background-color: #0079bf; color: white;
          border: none; border-radius: 4px;
        padding: 6px 10px; cursor: pointer;
      }
      .cancel-btn {
        background: none; border: none;
        color: #6b778c; cursor: pointer;
          padding: 6px 10px;
      }
      </style>
        </head>
        <body>
        
        <ul id="task-list" class="task-list"></ul>
        <button id="add-task" class="add-task-btn">+ Add New Task</button>
        
        <script>
        const t = TrelloPowerUp.iframe();
      let members = [];
      let checklistData = [];
      let isSaving = false;
      
      function loadCachedMembers() {
        t.get('board', 'shared', 'cachedMembers').then(cached => {
          if (Array.isArray(cached)) members = cached;
        });
      }
      
      function updateChecklistField(taskId, field, value) {
        t.get('card', 'shared', 'taskChecklist').then(existing => {
          const updated = Array.isArray(existing)
          ? existing.map(item => item.taskId === taskId ? { ...item, [field]: value } : item)
          : [];
          t.set('card', 'shared', { taskChecklist: updated }).then(() => {
            console.log(`✅ Task ${field} updated to Trello storage for taskId=${taskId}`);
          });
        });
      }
      
      function appendChecklistItem(task) {
        if (isSaving) return;
        isSaving = true;
        setTimeout(() => { isSaving = false }, 1000);
        
        t.get('card', 'shared', 'taskChecklist').then(existing => {
          const updated = Array.isArray(existing) ? [...existing, task] : [task];
          checklistData = updated;
          t.set('card', 'shared', { taskChecklist: updated }).then(() => {
            console.log('✅ Task appended to Trello storage:', task);
            refreshChecklistUI();
          });
        });
      }
      
      function createTaskInputRow() {
        const taskList = document.getElementById('task-list');
        const li = document.createElement('li');
        li.className = 'task-item';
        
        const inputRow = document.createElement('div');
        inputRow.className = 'task-input-row';
        
        const taskId = crypto.randomUUID();
        const task = { taskId, name: '', dueDate: '', memberAvatar: '' };
        
        const nameInput = document.createElement('input');
        nameInput.className = 'task-name';
        nameInput.placeholder = 'Task name...';
        
        const dateBtn = document.createElement('button');
        dateBtn.className = 'set-date';
        dateBtn.innerHTML = '<span>🕑</span> Set Date';
        dateBtn.addEventListener('click', (e) => {
          console.log('🕑 Date picker opened');
          t.popup({
            type: 'datetime',
            title: 'Due Date',
            date: new Date(),
            mouseEvent: e,
            callback: function(t, opts) {
              if (opts?.date) {
                const selected = new Date(opts.date).toLocaleDateString('en-US', {
                  month: 'short', day: 'numeric'
                });
                console.log('📅 Date selected:', selected);
                task.dueDate = selected;
                dateBtn.innerHTML = `🕑 ${selected}`;
                updateChecklistField(task.taskId, 'dueDate', selected);
              } else {
                console.log('📅 No date selected');
              }
            }
          });
        });
        
        const assignBtn = document.createElement('button');
        assignBtn.className = 'assign-member';
        assignBtn.innerHTML = '<span>👤</span> Assign';
        assignBtn.addEventListener('click', (e) => {
          console.log('👤 Member picker opened');
          t.popup({
            title: 'Assign Member',
            url: 'member-picker.html',
            height: 300,
            args: { members, taskId },
            mouseEvent: e,
            callback: function(t, selected) {
              if (selected?.name) {
                console.log(`[Member Picker] Selected: ${selected.name}`);
                task.memberAvatar = selected.name;
                assignBtn.innerHTML = `👤 ${selected.name}`;
                updateChecklistField(task.taskId, 'memberAvatar', selected.name);
              } else {
                console.log('🙅 No member selected');
              }
            }
          });
        });
        
        const addBtn = document.createElement('button');
        addBtn.className = 'add-btn';
        addBtn.textContent = 'Add';
        addBtn.addEventListener('click', () => {
          console.log('🔘 Add button clicked');
          task.name = nameInput.value;
          if (!task.name.trim()) {
            console.warn('⚠️ Task name is empty — not saving');
            return;
          }
          console.log('📦 Task being saved:', task);
          appendChecklistItem(task);
        });
        
        const cancelBtn = document.createElement('button');
        cancelBtn.className = 'cancel-btn';
        cancelBtn.textContent = 'Cancel';
        cancelBtn.addEventListener('click', () => li.remove());
        
        inputRow.appendChild(nameInput);
        inputRow.appendChild(dateBtn);
        inputRow.appendChild(assignBtn);
        inputRow.appendChild(addBtn);
        inputRow.appendChild(cancelBtn);
        
        li.appendChild(inputRow);
        taskList.appendChild(li);
      }
      
      function refreshChecklistUI() {
        const taskList = document.getElementById('task-list');
        taskList.innerHTML = '';
        checklistData.forEach(renderChecklistItem);
      }
      
      function renderChecklistItem(task) {
        const taskList = document.getElementById('task-list');
        const li = document.createElement('li');
        li.className = 'task-item';
        li.innerHTML = `
        <input type="text" class="task-name" value="${task.name || ''}" disabled />
          <button class="set-date">🕑 ${task.dueDate || 'No Date'}</button>
            <button class="assign-member">👤 ${task.memberAvatar || 'Unassigned'}</button>
              <button class="three-dots">⋮</button>
                `;
              taskList.appendChild(li);
      }
      
      let hasRendered = false;
      t.render(() => {
        if (hasRendered) return;
        hasRendered = true;
        
        console.log('🚀 Power-Up initialized on card open');
        console.log('👥 Loading cached board members...');
        loadCachedMembers();
        
        console.log('📦 Loading saved checklist from Trello...');
        t.get('card', 'shared', 'taskChecklist').then(saved => {
          if (Array.isArray(saved)) {
            console.log(`✅ Loaded ${saved.length} saved task(s) from storage`);
            checklistData = saved;
            refreshChecklistUI();
          }
        });
        
        document.getElementById('add-task').addEventListener('click', () => {
          console.log('➕ Add Task button clicked');
          createTaskInputRow();
        });
      });
      </script>
        </body>
        </html>
        
        
############################################################################ index.html
        <!DOCTYPE html>
        <html lang="en">
        <head>
        <meta charset="UTF-8" />
        <title>Task Checklist</title>
        <script src="https://p.trellocdn.com/power-up.min.js"></script>
        </head>
        <body>
        <script>
        const fetchMembersSilently = (t) => {
          console.log('🔄 Starting silent member fetch...');
          return t.board('id')
          .then(board =>
                  t.get('member', 'private', 'trello_token')
                .then(token => {
                  console.log('📡 Fetching members for board:', board.id);
                  return fetch(`https://api.trello.com/1/boards/${board.id}/members?key=7443867af11d32c3e8f8152b114201fd&token=${token}`);
                })
          )
          .then(res => res.json())
          .then(members => {
            console.log('✅ Members fetched:', members);
            return t.set('board', 'shared', { cachedMembers: members });
          })
          .catch(err => {
            console.error('❌ Failed to fetch members:', err);
          });
        };
      
      window.TrelloPowerUp.initialize({
        
        'authorization-status': t =>
          t.get('member', 'private', 'trello_token')
        .then(token => {
          console.log('🔐 Auth status:', !!token);
          return { authorized: !!token };
        }),
        
        'show-authorization': t =>
          t.popup({
            title: 'Authorize',
            url: './authorize.html',
            height: 150
          }),
        
        'card-back-section': (t) => ({
          title: 'Task Checklist',
          icon: 'https://birdieligos.github.io/trello-powerup-test/icon.jpeg',
          content: {
            type: 'iframe',
            url: t.signUrl('./client.html'),
            height: 500
          },
          action: {
            text: 'Add Task',
            callback: (t) => {
              console.log('🟦 "Add Task" button clicked — sending message to client');
              return t.postMessage({ type: 'add-task-inline' });
            }
          }
        }),
        
        'board-buttons': (t) => {
          console.log('📌 Board opened — fetching members silently');
          fetchMembersSilently(t);
          return [];
        },
        
        'popup': t => [],
        'card-buttons': t => [],
        'card-badges': t => [],
        'attachment-sections': t => [],
        'card-detail-badges': t => []
        
      });
      </script>
        </body>
        </html>
        
        
##############
    
      
      
      
##############MEMBER PCIKER 
      
      
      
      
      
      <!DOCTYPE html>
        <html>
        <head>
        <meta charset="utf-8" />
        <title>Select Member</title>
        <script src="https://p.trellocdn.com/power-up.min.js"></script>
        <style>
        ul { list-style: none; padding: 0; margin: 0; }
      li.member-item {
        margin-bottom: 10px;
        cursor: pointer;
        display: flex;
        align-items: center;
        padding: 6px;
        border-radius: 4px;
      }
      li.member-item.selected {
        background-color: #e4f0f6;
          font-weight: bold;
      }
      .checkmark {
        margin-left: auto;
        color: black;
        font-weight: bold;
      }
      </style>
        </head>
        <body style="font-family:sans-serif;padding:10px">
        <ul id="member-list"></ul>
        
        <script>
        const t = TrelloPowerUp.iframe();
      const list = document.getElementById('member-list');
      const taskId = t.arg('taskId');
      
      async function loadMembers() {
        try {
          const board = await t.board('id');
          const token = await t.get('member', 'private', 'trello_token');
          const res = await fetch(`https://api.trello.com/1/boards/${board.id}/members?key=7443867af11d32c3e8f8152b114201fd&token=${token}`);
          const members = await res.json();
          console.log(`[Member Picker] Loaded ${members.length} members`);
          
          members.sort((a, b) => a.fullName.localeCompare(b.fullName));
          
          members.forEach(m => {
            const li = document.createElement('li');
            li.className = 'member-item';
            li.innerHTML = `👤 ${m.fullName} <span class="checkmark" style="display:none">✓</span>`;
            
            li.addEventListener('click', () => {
              console.log(`[Member Picker] Selected: ${m.fullName}`);
              document.querySelectorAll('.member-item').forEach(el => {
                el.classList.remove('selected');
                el.querySelector('.checkmark').style.display = 'none';
              });
              li.classList.add('selected');
              li.querySelector('.checkmark').style.display = 'inline';
              
              setTimeout(() => {
                t.closePopup({ taskId, name: m.fullName });
              }, 300);
            });
            
            list.appendChild(li);
          });
          
        } catch (err) {
          list.innerHTML = `<li style="color:red">⚠️ Failed to load members</li>`;
          console.error('[Member Picker] Error loading members:', err);
        }
      }
      
      loadMembers();
      </script>
        </body>
        </html>

        
        
        
        
        
        
      