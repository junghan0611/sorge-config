# Work-context interface — branch handoff

Contract discussion and decisions live in [sorge-config #2](https://github.com/junghan0611/sorge-config/issues/2). This file is only the branch's next step; remove it before merge.

# RAIL — 현재 좌표

- [x] **1. 방향 합의와 공개 이슈** — 고정 카드·깊이가 아닌, 채널을 붙이고 질의에서 근거와 참조로 이어가는 읽기 계약.
- [ ] **2. 첫 소비자 검토와 범위 확정** ← CURRENT: 공개 이슈 스레드의 소비자 사례로 호출 흐름과 합성 검증 기준 조이기.
- [ ] **3. 명세와 합성 검증 한 묶음** ← PAUSED: 소비자 검토와 GLG 구현 승인 전에는 시작하지 않음.
- [ ] **4. 독립 소비자 재현** — 질의부터 다음 조회, 원 기록·담당 참조까지 별도 소비자가 실행하고 결과를 이슈에 남김.

# NOW

- Current: 운영·실무 코디가 방향을 합의했다. [첫 소비자 검토 질문](https://github.com/junghan0611/sorge-config/issues/2#issuecomment-7171958558)은 이슈에 남겼다. 스레드가 논의 기록이다.
- Next: 소비자 사례에서 유용한 첫 응답·다음 조회·멈출 지점을 고른다. 같은 호출자가 구조형 작업면과 시각이 있는 텍스트 채널을 바꿔 붙이는 검증으로 다듬고, GLG와 첫 구현 범위를 확정한다.
- Verify: 채널 교체가 호출자나 공용 봉투를 바꾸지 않고, 크기 제한·출처와 시점·누락과 읽기 실패·세션 상태 없는 이어 읽기·원 기록/담당 참조가 합성 예시에서 구분되는지 확인한다.
- Read: [공개 이슈와 스레드](https://github.com/junghan0611/sorge-config/issues/2). 이슈 본체가 계약 후보, 스레드가 그 변경 근거다.
- Do not touch: 실제 채널·자격정보·비공개 기록, 기존 forge 이관, 서버 배포·UI·자동 발신·전역 스킬 설치. 별도 요약 원장을 만들지 않는다.
